/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#ifndef AUDIO_MIDI_H
#define AUDIO_MIDI_H

#include "audio/mt32gm.h"

#include "common/config-manager.h"

MidiDriver_MT32GM::MidiDriver_MT32GM(MusicType midiType) :
		_driver(0),
		_nativeMT32(false),
		_enableGS(false),
		_isOpen(false),
		_outputChannelMask(65535), // Channels 1-16
		_baseFreq(250),
		_timerRate(0),
		_sysExDelay(0),
		_timer_param(0),
		_timer_proc(0) {
	switch (midiType) {
	case MT_MT32:
		_midiType = MT_MT32;
		break;
	case MT_GM:
	case MT_GS: // Treat GS same as GM
		_midiType = MT_GM;
		break;
	default:
		error("MidiDriver_MT32GM: Unsupported music type %i", midiType);
		break;
	}

	for (int i = 0; i < MAXIMUM_SOURCES; ++i) {
		// Default MIDI channel mapping: data channel == output channel
		for (int j = 0; j < MIDI_CHANNEL_COUNT; ++j) {
			_sources[i].channelMap[j] = j;
		}
	}

	_maximumActiveNotes = _midiType == MT_MT32 ? MAXIMUM_MT32_ACTIVE_NOTES : MAXIMUM_GM_ACTIVE_NOTES;
	_activeNotes = new ActiveNote[_maximumActiveNotes];
	assert(_activeNotes);
}

MidiDriver_MT32GM::~MidiDriver_MT32GM() {
	Common::StackLock lock(_mutex);
	if (_driver) {
		_driver->setTimerCallback(0, 0);
		_driver->close();
		delete _driver;
	}
	_driver = 0;

	if (_activeNotes)
		delete[] _activeNotes;
}

int MidiDriver_MT32GM::open() {
	assert(!_driver);

	// Setup midi driver
	MidiDriver::DeviceHandle dev = MidiDriver::detectDevice(MDT_MIDI | (_midiType == MT_MT32 ? MDT_PREFER_MT32 : MDT_PREFER_GM));
	MusicType deviceMusicType = MidiDriver::getMusicType(dev);
	if (!(deviceMusicType == MT_MT32 || deviceMusicType == MT_GM || deviceMusicType == MT_GS))
		error("MidiDriver_MT32GM: detected music device uses unsupported music type %i", deviceMusicType);

	MidiDriver *driver = MidiDriver::createMidi(dev);
	bool nativeMT32 = deviceMusicType == MT_MT32 || ConfMan.getBool("native_mt32");

	return open(driver, nativeMT32);
}

int MidiDriver_MT32GM::open(MidiDriver *driver, bool nativeMT32) {
	assert(!_driver);

	_driver = driver;
	_nativeMT32 = nativeMT32;

	_enableGS = ConfMan.getBool("enable_gs");

	if (!_driver)
		return 255;

	if (_nativeMT32)
		_outputChannelMask = _midiType == MT_MT32 ? 1022 : 767; // Channels 2-10 / 1-8 and 10
	_driver->property(MidiDriver::PROP_CHANNEL_MASK, _outputChannelMask);

	int ret = _driver->open();
	if (ret != MidiDriver::MERR_ALREADY_OPEN && ret != 0)
		return ret;

	_timerRate = _driver->getBaseTempo();
	_driver->setTimerCallback(this, timerCallback);

	initMidiDevice();

	_isOpen = true;

	return 0;
}

void MidiDriver_MT32GM::initMidiDevice() {
	if (_nativeMT32) {
		initMT32(_midiType != MT_MT32);
	} else {
		initGM(_midiType == MT_MT32, _enableGS);
	}
}

void MidiDriver_MT32GM::close() {
	if (_driver) {
		_driver->close();
	}
}

void MidiDriver_MT32GM::send(uint32 b) {
	send(-1, b);
}

// MIDI messages can be found at http://www.midi.org/techspecs/midimessages.php
void MidiDriver_MT32GM::send(int8 source, uint32 b) {
	assert(source < MAXIMUM_SOURCES);

	byte command = b & 0xf0;
	byte dataChannel = b & 0xf;
	byte outputChannel = source < 0 ? dataChannel : _sources[source].channelMap[dataChannel];
	if (outputChannel == -1) {
		for (int i = 0; i < MIDI_CHANNEL_COUNT; ++i) {
			if ((_sources[source].availableChannels >> i) & 1) {
				_sources[source].availableChannels &= ~(1 << i);
				_sources[source].channelMap[dataChannel] = i;
				outputChannel = i;
				break;
			}
		}
		if (outputChannel == -1) {
			warning("MidiDriver_MT32GM: Insufficient available channels for source %i", source);
			return;
		}
	}
	MidiChannelControlData &controlData = _controlData[outputChannel];
	byte op1 = (b >> 8) & 0xff;
	byte op2 = (b >> 16) & 0xff;

	if (command != 0xF0 && controlData.source != source) {
		// A new source has sent an event on this channel.
		controlData.sourceVolumeApplied = false;
		controlData.source = source;
	}

	switch (command) {
	case 0x80: // Note Off
	case 0x90: // Note On
		noteOnOff(outputChannel, command, op1, op2, source, controlData);
		break;
	case 0xa0: // Polyphonic key pressure (aftertouch) (not supported by MT-32 or GM)
	case 0xd0: // Channel pressure (aftertouch) (not supported by MT-32)
	case 0xe0: // pitch bend change
		_driver->send(command | outputChannel, op1, op2);
		break;
	case 0xb0: // Control change
		controlChange(outputChannel, op1, op2, source, controlData);
		break;
	case 0xc0: // Program Change
		programChange(outputChannel, op1, source, controlData);
		break;
	case 0xf0: // SysEx
		// SysExes should be sent using the sysEx functions and are not processed here.
		warning("MidiDriver_MT32GM: send received SysEx (not processed): %x", b);
		break;
	default:
		warning("MidiDriver_MT32GM: Received unknown event %02x", command);
		break;
	}
}

void MidiDriver_MT32GM::noteOnOff(byte outputChannel, byte command, byte note, byte velocity, int8 source, MidiChannelControlData &controlData) {
	// Note On with velocity 0 is treated as Note Off
	bool addNote = command == 0x90 && velocity != 0;
	if (addNote) {
		if (source >= 0 && !controlData.sourceVolumeApplied)
			// Source volume hasn't been applied yet. Do so now.
			controlChange(outputChannel, MIDI_CONTROLLER_VOLUME, controlData.volume, source, controlData);
		// Add the new note to the active note registration
		for (int i = 0; i < _maximumActiveNotes; ++i) {
			ActiveNote &activeNote = _activeNotes[i];
			if (activeNote.channel == 0xFF) {
				// Add the new note.
				activeNote.source = source;
				activeNote.channel = outputChannel;
				activeNote.note = note;
				activeNote.sustain = false;
				break;
			}
		}
	} else {
		// Remove the note from the active note registration
		for (int i = 0; i < _maximumActiveNotes; ++i) {
			ActiveNote &activeNote = _activeNotes[i];
			if (activeNote.channel == outputChannel && activeNote.source == source && activeNote.note == note) {
				if (controlData.sustain) {
					// Sustain is on, so the note should be turned off
					// when sustain is turned off.
					activeNote.sustain = true;
				} else {
					// Turn off the existing note.
					activeNote.source = 0x7F;
					activeNote.channel = 0xFF;
				}
				break;
			}
		}
	}

	_driver->send(command | outputChannel, note, velocity);
}

void MidiDriver_MT32GM::controlChange(byte outputChannel, byte controllerNumber, byte controllerValue, int8 source, MidiChannelControlData &controlData) {
	assert(source < MAXIMUM_SOURCES);

	// Standard MIDI controllers
	switch (controllerNumber) {
	case MIDI_CONTROLLER_BANK_SELECT_MSB:
		// Keep track of the current bank for each channel
		_gsBank[outputChannel] = controllerValue;
		break;
	case MIDI_CONTROLLER_VOLUME:
		controlData.volume = controllerValue;
		controlData.sourceVolumeApplied = true;
		if (source >= 0) {
			// Scale to source volume
			controllerValue = (_sources[source].volume * controllerValue) >> 8;
		}
		if (_scaleGSPercussionVolumeToMT32 && outputChannel == MIDI_RHYTHM_CHANNEL) {
			// Scale GS percussion channel volume to MT-32 level (80/127)
			controllerValue = (80 * controllerValue) >> 7;
		}
		if (controlData.scaledVolume == controllerValue) {
			// Volume is already at this value, so no need to send it out
			// to the MIDI device.
			return;
		}
		controlData.scaledVolume = controllerValue;
		break;
	case MIDI_CONTROLLER_PANNING:
		if (_reversePanning) {
			// Center panning is 0x40
			controllerValue = 0x80 - controllerValue;
			if (controllerValue > 0x7F)
				controllerValue = 0x7F;
		}
		break;
	case MIDI_CONTROLLER_SUSTAIN:
		controlData.sustain = controllerValue >= 0x40;
		if (!controlData.sustain) {
			removeActiveNotes(outputChannel, true);
		}
		break;
	case MIDI_CONTROLLER_RESET_ALL_CONTROLLERS:
		controlData.sustain = false;
		removeActiveNotes(outputChannel, true);
		break;
	case MIDI_CONTROLLER_OMNI_ON:
	case MIDI_CONTROLLER_OMNI_OFF:
	case MIDI_CONTROLLER_MONO_ON:
	case MIDI_CONTROLLER_POLY_ON:
		// These act as an All Notes Off on MT-32, but also turn sustain off.
		// They are not part of GM, so should not be used in GM data.
		if (_midiType != MT_MT32) {
			warning("MidiDriver_MT32GM: unsupported GM controller %x", controllerNumber);
			return;
		}

		controlData.sustain = false;
		removeActiveNotes(outputChannel, true);
		if (!_nativeMT32) {
			// MT-32 data on GM device.
			// These controllers might not be supported or have side effects
			// (changing omni or mono/poly mode). Send All Notes Off and
			// Sustain Off instead.
			controllerNumber = MIDI_CONTROLLER_ALL_NOTES_OFF;
			_driver->send(0xB0 | outputChannel | (MIDI_CONTROLLER_SUSTAIN << 8) | (0 << 16));
		}
		// fall through
	case MIDI_CONTROLLER_ALL_NOTES_OFF:
		removeActiveNotes(outputChannel, false);
		break;
	default:
		break;
	}

	_driver->send(0xB0 | outputChannel | (controllerNumber << 8) | (controllerValue << 16));
}

void MidiDriver_MT32GM::removeActiveNotes(uint8 outputChannel, bool sustainedNotes) {
	// Remove sustained notes from the active notes registration
	for (int i = 0; i < _maximumActiveNotes; ++i) {
		if (_activeNotes[i].channel == outputChannel && _activeNotes[i].sustain == sustainedNotes) {
			_activeNotes[i].source = 0x7F;
			_activeNotes[i].channel = 0xFF;
		}
	}
}

void MidiDriver_MT32GM::programChange(byte outputChannel, byte patchId, int8 source, MidiChannelControlData &controlData) {
	// remember patch id for the current MIDI-channel
	controlData.program = patchId;

	if (_midiType == MT_MT32) {
		if (outputChannel == MIDI_RHYTHM_CHANNEL)
			// Patch changes on the rhythm channel do nothing on an MT-32.
			// On GM/GS devices they might unintentionally change the drumkit.
			return;

		if (!_nativeMT32 && !_enableGS) {
			// GM device: map the patch to GM equivalent
			patchId = _mt32ToGm[patchId];
		}
	} else {
		// GM/GS MIDI
		if (outputChannel == MIDI_RHYTHM_CHANNEL) {
			// Correct possible wrong GS drumkit number
			patchId = _gsDrumkitFallbackMap[patchId];
		} else if (!_nativeMT32) {
			// Correct possible wrong bank / instrument variation
			byte correctedBank = correctInstrumentBank(outputChannel, patchId);
			if (correctedBank != 0xFF) {
				// Send out a bank select for the corrected bank number
				controlChange(outputChannel, MIDI_CONTROLLER_BANK_SELECT_MSB, correctedBank, source, controlData);
				controlChange(outputChannel, MIDI_CONTROLLER_BANK_SELECT_LSB, 0, source, controlData);
			}
		} else {
			// GM on an MT-32: map the patch to the MT-32 equivalent
			patchId = _gmToMt32[patchId];
		}
	}

	// Finally send program change to MIDI device
	_driver->send(0xC0 | outputChannel | (patchId << 8));
}

void MidiDriver_MT32GM::sysEx(const byte *msg, uint16 length) {
	uint16 delay = sysExNoDelay(msg, length);

	if (delay > 0)
		g_system->delayMillis(delay);
}

uint16 MidiDriver_MT32GM::sysExNoDelay(const byte *msg, uint16 length) {
	if (!_nativeMT32 && length >= 3 && msg[0] == 0x41 && msg[2] == 0x16)
		// MT-32 SysExes have no effect on GM devices.
		return 0;

	// Send SysEx
	_driver->sysEx(msg, length);

	// Wait the time it takes to send the SysEx data
	uint16 delay = (length + 2) * 1000 / 3125;

	// Plus an additional delay for the MT-32 rev00
	if (_nativeMT32)
		delay += 40;

	return delay;
}

void MidiDriver_MT32GM::sysExQueue(const byte *msg, uint16 length) {
	SysExData sysEx;
	memcpy(sysEx.data, msg, length);
	sysEx.length = length;

	_sysExQueueMutex.lock();
	_sysExQueue.push(sysEx);
	_sysExQueueMutex.unlock();
}

uint16 MidiDriver_MT32GM::sysExMT32(const byte *msg, uint16 length, const uint32 targetAddress, bool queue, bool delay) {
	byte   sysExMessage[270];
	uint16 sysExPos = 0;
	byte   sysExByte;
	uint16 sysExChecksum = 0;

	memset(&sysExMessage, 0, sizeof(sysExMessage));

	sysExMessage[0] = 0x41; // Roland
	sysExMessage[1] = 0x10;
	sysExMessage[2] = 0x16; // Model MT32
	sysExMessage[3] = 0x12; // Command DT1

	sysExChecksum = 0;

	sysExMessage[4] = (targetAddress >> 14) & 0x7F;
	sysExMessage[5] = (targetAddress >> 7) & 0x7F;
	sysExMessage[6] = targetAddress & 0x7F;

	for (byte targetAddressByte = 4; targetAddressByte < 7; targetAddressByte++) {
		assert(sysExMessage[targetAddressByte] < 0x80); // security check
		sysExChecksum -= sysExMessage[targetAddressByte];
	}

	sysExPos = 7;
	for (int i = 0; i < length; ++i) {
		sysExByte = *msg++;

		assert(sysExPos < sizeof(sysExMessage));
		assert(sysExByte < 0x80); // security check
		sysExMessage[sysExPos++] = sysExByte;
		sysExChecksum -= sysExByte;
	}

	// Calculate checksum
	assert(sysExPos < sizeof(sysExMessage));
	sysExMessage[sysExPos++] = sysExChecksum & 0x7F;

	if (queue) {
		sysExQueue(sysExMessage, sysExPos);
	} else if (!delay) {
		return sysExNoDelay(sysExMessage, sysExPos);
	} else {
		sysEx(sysExMessage, sysExPos);
	}

	return 0;
}

void MidiDriver_MT32GM::metaEvent(int8 source, byte type, byte *data, uint16 length) {
	assert(source < MAXIMUM_SOURCES);

	if (type == 0x2F && source >= 0) // End of Track
		deinitSource(source);

	_driver->metaEvent(type, data, length);
}

void MidiDriver_MT32GM::stopAllNotes(bool stopSustainedNotes) {
	for (int i = 0; i < MIDI_CHANNEL_COUNT; ++i) {
		if (!isOutputChannelUsed(i))
			continue;

		if (stopSustainedNotes) {
			_driver->send(0xB0 | i, MIDI_CONTROLLER_SUSTAIN, 0);
			_controlData[i].sustain = false;
		}
		_driver->send(0xB0 | i, MIDI_CONTROLLER_ALL_NOTES_OFF, 0);
	}
	for (int i = 0; i < _maximumActiveNotes; ++i) {
		if (stopSustainedNotes || !_activeNotes[i].sustain) {
			_activeNotes[i].source = 0x7F;
			_activeNotes[i].channel = 0xFF;
		}
	}
}

MidiChannel *MidiDriver_MT32GM::allocateChannel() {
	if (_driver)
		return _driver->allocateChannel();
	return 0;
}

MidiChannel *MidiDriver_MT32GM::getPercussionChannel() {
	if (_driver)
		return _driver->getPercussionChannel();
	return 0;
}

uint32 MidiDriver_MT32GM::getBaseTempo() {
	if (_driver) {
		return _driver->getBaseTempo();
	}
	return 1000000 / _baseFreq;
}

bool MidiDriver_MT32GM::allocateSourceChannels(uint8 source, uint8 numChannels) {
	assert(source < MAXIMUM_SOURCES);

	deinitSource(source);

	uint16 claimedChannels = 0;
	if (numChannels > 0) {
		for (int i = 0; i < MIDI_CHANNEL_COUNT; ++i) {
			if (!isOutputChannelUsed(i))
				continue;

			if (_controlData[i].source == -1) {
				claimedChannels |= (1 << i);
				numChannels--;
			}
			if (numChannels == 0)
				break;
		}
	}

	if (numChannels > 0)
		// Not enough channels available.
		return false;

	// Allocate the channels.
	for (int i = 0; i < MIDI_CHANNEL_COUNT; ++i) {
		if ((claimedChannels >> i) & 1) {
			_controlData[i].source = source;
		}
		// Clear the source channel mapping.
		_sources[source].channelMap[i] = -1;
	}
	_sources[source].availableChannels = claimedChannels;

	return true;
}

void MidiDriver_MT32GM::deinitSource(uint8 source) {
	assert(source < MAXIMUM_SOURCES);

	// Free channels which were used by this source.
	for (int i = 0; i < MIDI_CHANNEL_COUNT; ++i) {
		if (!isOutputChannelUsed(i))
			continue;

		if (_controlData[i].source == source)
			_controlData[i].source = -1;

	}
	_sources[source].availableChannels = 0xFFFF;
	// Reset the data to output channel mapping
	for (int i = 0; i < MIDI_CHANNEL_COUNT; ++i) {
		_sources[source].channelMap[i] = i;
	}
	// Stop any active notes.
	for (int i = 0; i < _maximumActiveNotes; ++i) {
		if (_activeNotes[i].source == source) {
			if (_activeNotes[i].sustain) {
				// Turn off sustain
				controlChange(_activeNotes[i].channel, MIDI_CONTROLLER_SUSTAIN, 0x00, source, _controlData[i]);
			} else {
				// Send note off
				noteOnOff(_activeNotes[i].channel, 0x80, _activeNotes[i].note, 0x00, source, _controlData[i]);
			}
		}
	}
}

void MidiDriver_MT32GM::setSourceVolume(uint8 source, uint16 volume) {
	assert(source < MAXIMUM_SOURCES);

	_sources[source].volume = volume;

	for (int i = 0; i < MIDI_CHANNEL_COUNT; ++i) {
		if (!isOutputChannelUsed(i))
			continue;

		if (_controlData[i].source == source && _controlData[i].volume != 0xFF)
			controlChange(i, MIDI_CONTROLLER_VOLUME, _controlData[i].volume, source, _controlData[i]);
	}
}

void MidiDriver_MT32GM::onTimer() {
	Common::StackLock lock(_sysExQueueMutex);

	_sysExDelay -= (_sysExDelay > _timerRate) ? _timerRate : _sysExDelay;

	if (!_sysExQueue.empty() && _sysExDelay == 0) {
		// Ready to send next SysEx message to the MIDI device
		SysExData sysEx = _sysExQueue.pop();
		_sysExDelay = sysExNoDelay(sysEx.data, sysEx.length) * 1000;
	}
}

#endif

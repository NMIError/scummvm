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

#ifndef AUDIO_MT32GM_H
#define AUDIO_MT32GM_H

#include "audio/mididrv.h"
#include "common/mutex.h"
#include "common/queue.h"

class MidiDriver_MT32GM : public MidiDriver {
public:
	static const uint8 MIDI_CHANNEL_COUNT = 16;
	static const uint8 MIDI_RHYTHM_CHANNEL = 9;

	static const byte MIDI_CONTROLLER_BANK_SELECT_MSB = 0x00;
	static const byte MIDI_CONTROLLER_VOLUME = 0x07;
	static const byte MIDI_CONTROLLER_PANNING = 0x0A;
	static const byte MIDI_CONTROLLER_BANK_SELECT_LSB = 0x20;
	static const byte MIDI_CONTROLLER_SUSTAIN = 0x40;
	static const byte MIDI_CONTROLLER_RESET_ALL_CONTROLLERS = 0x79;
	static const byte MIDI_CONTROLLER_ALL_NOTES_OFF = 0x7B;
	static const byte MIDI_CONTROLLER_OMNI_ON = 0x7C;
	static const byte MIDI_CONTROLLER_OMNI_OFF = 0x7D;
	static const byte MIDI_CONTROLLER_MONO_ON = 0x7E;
	static const byte MIDI_CONTROLLER_POLY_ON = 0x7F;
	
	static const uint8 MAXIMUM_SOURCES = 4;
	static const uint8 MAXIMUM_MT32_ACTIVE_NOTES = 48;
	static const uint8 MAXIMUM_GM_ACTIVE_NOTES = 96;

protected:
	/**
	 * This stores the values of the MIDI controllers for
	 * a MIDI channel. It is used to keep track of controller
	 * values while a channel is locked, so they can be
	 * restored when the channel is unlocked.
	 */
	struct MidiChannelControlData {
		// The source that last sent an event to this channel
		int8 source;
		// True if the source volume has been applied to this channel
		bool sourceVolumeApplied;

		byte program;
		// The volume specified by the MIDI data
		byte volume;
		// The volume scaled using the source volume
		byte scaledVolume;
		bool sustain;

		MidiChannelControlData() : source(-1),
			sourceVolumeApplied(false),
			program(0),
			volume(0xFF),
			scaledVolume(0x64),
			sustain(false) { }
	};

	/**
	 * This stores data about a specific source of MIDI data.
	 */
	struct MidiSource {
		// The source volume as set by ScummVM (music/SFX volume)
		uint16 volume;
		// The mapping of MIDI data channels to output channels
		// for this source.
		int8 channelMap[MIDI_CHANNEL_COUNT];
		uint16 availableChannels;

		MidiSource() : volume(256), availableChannels(0xFFFF) {
			memset(channelMap, 0, sizeof(channelMap));
		}
	};

	struct ActiveNote {
		int8 source;
		uint8 channel;
		uint8 note;
		bool sustain;

		ActiveNote() : source(0x7F),
			channel(0xFF),
			note(0xFF),
			sustain(false) { }
	};

	/**
	 * Stores data which is to be transmitted as a SysEx message
	 * to a MIDI device. Neither data nor length should include
	 * the SysEx start and stop bytes.
	 */
	struct SysExData {
		byte data[270];
		uint16 length;
		SysExData() : length(0) {
			memset(data, 0, sizeof(data));
		}
	};

public:
	MidiDriver_MT32GM(MusicType midiType);
	virtual ~MidiDriver_MT32GM();

	// MidiDriver interface
	int open() override;
	// Open the driver wrapping the specified MidiDriver instance.
	int open(MidiDriver *driver, bool nativeMT32);
	void close() override;
	bool isOpen() const override { return _isOpen; }
	bool isReady() override { return _sysExQueue.empty(); }

	using MidiDriver_BASE::send;
	void send(uint32 b) override;
	void send(int8 source, uint32 b) override;
	void sysEx(const byte *msg, uint16 length) override;
	uint16 sysExNoDelay(const byte *msg, uint16 length) override;
	void sysExQueue(const byte *msg, uint16 length);
	uint16 sysExMT32(const byte *msg, uint16 length, const uint32 targetAddress, bool queue = false, bool delay = false);
	void metaEvent(int8 source, byte type, byte *data, uint16 length) override;

	void stopAllNotes(bool stopSustainedNotes = false) override;
	MidiChannel *allocateChannel() override;
	MidiChannel *getPercussionChannel() override;
	uint32 getBaseTempo() override;
	
	void noteOnOff(byte command, byte outputChannel, byte note, byte velocity, int8 source, MidiChannelControlData &controlData);
	/**
	 * Send out a control change MIDI message using the specified data.
	 * @param controlData The new MIDI controller value will be set on this MidiChannelControlData
	 * @param sendMessage True if the message should be sent out to the device
	 */
	void controlChange(byte outputChannel, byte controllerNumber, byte controllerValue, int8 source, MidiChannelControlData &controlData);
	/**
	 * Send a program change MIDI message using the specified data.
	 * @param controlData The new program value will be set on this MidiChannelControlData
	 * @param sendMessage True if the message should be sent out to the device
	 */
	void programChange(byte outputChannel, byte patchId, int8 source, MidiChannelControlData &controlData);

	bool allocateSourceChannels(uint8 source, uint8 numChannels);
	void deinitSource(uint8 source);
	void setSourceVolume(uint8 source, uint16 volume);

	void setTimerCallback(void *timer_param, Common::TimerManager::TimerProc timer_proc) override {
		_timer_param = timer_param;
		_timer_proc = timer_proc;
	}
	void onTimer();

protected:
	void initMidiDevice();
	void removeActiveNotes(uint8 outputChannel, bool sustainedNotes);
	bool isOutputChannelUsed(uint8 outputChannel) { return _outputChannelMask & (1 << outputChannel); }

	Common::Mutex _mutex;

	MidiDriver *_driver;
	MusicType _midiType;
	bool _nativeMT32;
	bool _enableGS;

	bool _isOpen;
	// Bitmask of the MIDI channels in use by the output device
	uint16 _outputChannelMask;
	int _baseFreq;
	uint32 _timerRate;

	// stores the controller values for each MIDI channel
	MidiChannelControlData _controlData[MIDI_CHANNEL_COUNT];

	MidiSource _sources[MAXIMUM_SOURCES];

	uint8 _maximumActiveNotes;
	ActiveNote *_activeNotes;

	// The number of microseconds to wait before sending the
	// next SysEx message.
	uint32 _sysExDelay;
	/**
	 * Queue of SysEx messages that must be sent to the
	 * MIDI device. Used by processXMIDITimbreChunk to
	 * send SysEx messages before starting playback of
	 * a track.
	 *
	 * Sending other MIDI messages to the driver should
	 * be suspended until all SysEx messages in the
	 * queue have been sent to the MIDI device. Use the
	 * isReady function to check if the driver is ready
	 * to receive other messages.
	 */
	Common::Queue<SysExData> _sysExQueue;
	// Mutex for write access to the SysEx queue.
	Common::Mutex _sysExQueueMutex;

	// External timer callback
	void *_timer_param;
	Common::TimerManager::TimerProc _timer_proc;

public:
	// Callback hooked up to the driver wrapped by the MIDI driver
	// object. Executes onTimer and the external callback set by
	// the setTimerCallback function.
	static void timerCallback(void *data) {
		MidiDriver_MT32GM *driver = (MidiDriver_MT32GM *)data;
		driver->onTimer();
		if (driver->_timer_proc && driver->_timer_param)
			driver->_timer_proc(driver->_timer_param);
	}
};

#endif

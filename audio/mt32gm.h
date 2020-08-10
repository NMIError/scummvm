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
	static const uint8 MAXIMUM_SOURCES = 10;

	static const byte MT32_DEFAULT_INSTRUMENTS[8];
	static const byte MT32_DEFAULT_PANNING[8];
	// Map for correcting Roland GS drumkit numbers.
	static const uint8 GS_DRUMKIT_FALLBACK_MAP[128];

protected:
	static const uint8 MAXIMUM_MT32_ACTIVE_NOTES = 48;
	static const uint8 MAXIMUM_GM_ACTIVE_NOTES = 96;

	static const uint16 FADING_DELAY = 25 * 1000;

public:
	enum SourceType {
		SOURCE_TYPE_UNDEFINED,
		SOURCE_TYPE_MUSIC,
		SOURCE_TYPE_SFX
	};

	enum FadeAbortType {
		FADE_ABORT_TYPE_END_VOLUME,
		FADE_ABORT_TYPE_CURRENT_VOLUME,
		FADE_ABORT_TYPE_START_VOLUME
	};
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

		uint16 pitchWheel;
		byte program;
		byte instrumentBank;

		byte modulation;
		// The volume specified by the MIDI data
		byte volume;
		// The volume scaled using the source volume
		byte scaledVolume;
		byte panPosition;
		byte expression;
		bool sustain;

		MidiChannelControlData() : source(-1),
			sourceVolumeApplied(false),
			pitchWheel(MIDI_PITCH_BEND_DEFAULT),
			program(0),
			instrumentBank(0),
			modulation(0),
			volume(0x64),
			scaledVolume(0x64),
			panPosition(0x40),
			expression(0x7F),
			sustain(false) { }
	};

	/**
	 * This stores data about a specific source of MIDI data.
	 */
	struct MidiSource {
		SourceType type;
		// The source volume (relative volume for this source as defined by the game).
		// Default is the default neutral value (255).
		uint16 volume;
		// The source volume level at which no scaling is performed (volume as defined
		// in MIDI data is used directly). Volume values below this decrease volume,
		// values above increase volume (up to the maximum MIDI channel volume).
		// Set this to match the volume values used by the game engine to avoid having
		// to convert them. Default value is 255; minimum value is 1.
		uint16 neutralVolume;
		uint16 fadeStartVolume;
		uint16 fadeEndVolume;
		int32 fadePassedTime;
		int32 fadeDuration;
		// The mapping of MIDI data channels to output channels
		// for this source.
		int8 channelMap[MIDI_CHANNEL_COUNT];
		uint16 availableChannels;

		MidiSource() : type(SOURCE_TYPE_UNDEFINED), volume(256), neutralVolume(256), fadeStartVolume(0),
				fadeEndVolume(0), fadePassedTime(0), fadeDuration(0), availableChannels(0xFFFF) {
			memset(channelMap, 0, sizeof(channelMap));
		}
	};

	struct ActiveNote {
		int8 source;
		uint8 channel;
		uint8 note;
		bool sustain;

		ActiveNote() { clear(); }

		void clear() {
			source = 0x7F;
			channel = 0xFF;
			note = 0xFF;
			sustain = false;
		}

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
	~MidiDriver_MT32GM();

	// MidiDriver interface
	int open() override;
	// Open the driver wrapping the specified MidiDriver instance.
	virtual int open(MidiDriver *driver, bool nativeMT32);
	void close() override;
	bool isOpen() const override { return _isOpen; }
	bool isReady() override { return _sysExQueue.empty(); }
	uint32 property(int prop, uint32 param) override;

	using MidiDriver_BASE::send;
	void send(uint32 b) override;
	void send(int8 source, uint32 b) override;
	void sysEx(const byte *msg, uint16 length) override;
	uint16 sysExNoDelay(const byte *msg, uint16 length) override;
	void sysExQueue(const byte *msg, uint16 length);
	uint16 sysExMT32(const byte *msg, uint16 length, const uint32 targetAddress, bool queue = false, bool delay = true);
	void metaEvent(int8 source, byte type, byte *data, uint16 length) override;

	void stopAllNotes(bool stopSustainedNotes = false) override;
	void startFade(uint16 duration, uint16 targetVolume);
	void startFade(uint8 source, uint16 duration, uint16 targetVolume);
	void abortFade(FadeAbortType abortType = FADE_ABORT_TYPE_END_VOLUME);
	void abortFade(uint8 source, FadeAbortType abortType = FADE_ABORT_TYPE_END_VOLUME);
	bool isFading();
	bool isFading(uint8 source);
	void clearSysExQueue();
	MidiChannel *allocateChannel() override;
	MidiChannel *getPercussionChannel() override;
	uint32 getBaseTempo() override;
	
	virtual bool allocateSourceChannels(uint8 source, uint8 numChannels);
	virtual void deinitSource(uint8 source);
	void setSourceType(SourceType type);
	void setSourceType(uint8 source, SourceType type);
	void setSourceVolume(uint16 volume);
	virtual void setSourceVolume(uint8 source, uint16 volume);
	void setSourceNeutralVolume(uint16 volume);
	void setSourceNeutralVolume(uint8 source, uint16 volume);
	void syncSoundSettings();

	void setTimerCallback(void *timer_param, Common::TimerManager::TimerProc timer_proc) override {
		_timer_param = timer_param;
		_timer_proc = timer_proc;
	}
	virtual void onTimer();

protected:
	virtual void initControlData();
	virtual void initMidiDevice();
	/**
	 * Initializes the MT-32 MIDI device. The device will be reset and,
	 * if the parameter is specified, set up for General MIDI data.
	 * @param initForGM True if the MT-32 should be initialized for GM mapping
	 */
	virtual void initMT32(bool initForGM);
	/**
	 * Initializes the General MIDI device. The device will be reset.
	 * If the initForMT32 parameter is specified, the device will be set up for
	 * MT-32 MIDI data. If the device supports Roland GS, the enableGS
	 * parameter can be specified for enhanced GS MT-32 compatiblity.
	 * @param initForMT32 True if the device should be initialized for MT-32 mapping
	 * @param enableGS True if the device should be initialized for GS MT-32 mapping
	 */
	virtual void initGM(bool initForMT32, bool enableGS);
	virtual void processEvent(int8 source, uint32 b, uint8 outputChannel,
		MidiChannelControlData &controlData, bool channelLockedByOtherSource = false);
	virtual void noteOnOff(byte command, byte outputChannel, byte note, byte velocity,
		int8 source, MidiChannelControlData &controlData);
	/**
	 * Send out a control change MIDI message using the specified data.
	 * @param controlData The new MIDI controller value will be set on this MidiChannelControlData
	 * @param sendMessage True if the message should be sent out to the device
	 */
	virtual void controlChange(byte outputChannel, byte controllerNumber, byte controllerValue,
		int8 source, MidiChannelControlData &controlData, bool channelLockedByOtherSource = false);
	/**
	 * Send a program change MIDI message using the specified data.
	 * @param controlData The new program value will be set on this MidiChannelControlData
	 * @param sendMessage True if the message should be sent out to the device
	 */
	virtual void programChange(byte outputChannel, byte patchId, int8 source,
		MidiChannelControlData &controlData, bool channelLockedByOtherSource = false);
	virtual bool addActiveNote(uint8 outputChannel, uint8 note, int8 source);
	virtual bool removeActiveNote(uint8 outputChannel, uint8 note, int8 source);
	virtual void removeActiveNotes(uint8 outputChannel, bool sustainedNotes);
	bool isOutputChannelUsed(int8 outputChannel);
	/**
	 * Checks if the currently selected GS bank / instrument variation
	 * on the specified channel is valid for the specified patch.
	 * If this is not the case, the correct bank will be returned which
	 * can be set by sending a bank select message. If no correction is
	 * needed, 0xFF will be returned.
	 * This emulates the fallback functionality of the Roland SC-55 v1.2x,
	 * on which some games rely to correct wrong bank selects.
	 */
	byte correctInstrumentBank(byte outputChannel, byte patchId);

	void updateFading();

	virtual int8 mapSourceChannel(uint8 source, uint8 dataChannel);

	Common::Mutex _fadingMutex;
	Common::Mutex _allocationMutex;
	Common::Mutex _activeNotesMutex;

	MidiDriver *_driver;
	// The type of MIDI data supplied to the driver: MT-32 or General MIDI.
	MusicType _midiType;
	// True if the MIDI output is an MT-32 (hardware or 100% emulated),
	// false if the MIDI output is a General MIDI device.
	bool _nativeMT32;
	// True if the General MIDI output supports Roland GS for improved MT-32 mapping.
	bool _enableGS;
	// Indicates if the stereo panning in the MIDI data is reversed
	// compared to the stereo panning of the intended MIDI device
	bool _midiDataReversePanning;
	// Indicates if the stereo panning of the output MIDI device is
	// reversed compared to the stereo panning of the type of MIDI
	// device used by the game (i.e. MT-32 data playing on a GM
	// device or the other way around).
	bool _midiDeviceReversePanning;
	// True if GS percussion channel volume should be scaled to match MT-32 volume.
	bool _scaleGSPercussionVolumeToMT32;

	bool _userVolumeScaling;

	uint16 _userMusicVolume;
	uint16 _userSfxVolume;
	bool _userMute;

	bool _isOpen;
	// Bitmask of the MIDI channels in use by the output device
	uint16 _outputChannelMask;
	int _baseFreq;
	uint32 _timerRate;

	// stores the controller values for each MIDI channel
	MidiChannelControlData *_controlData[MIDI_CHANNEL_COUNT];

	MidiSource _sources[MAXIMUM_SOURCES];

	uint8 _maximumActiveNotes;
	ActiveNote *_activeNotes;

	// The number of microseconds to wait before the next
	// fading step.
	uint16 _fadeDelay;

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

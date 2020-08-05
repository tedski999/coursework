
public class AudioEngine
{
	PApplet app;
	Sound sound;
	LowPass lowPass;
	SoundFile currentPlayer;
	ArrayList<SynthSound> synthSounds;

	public AudioEngine(PApplet app)
	{
		this.app = app;
		sound = new Sound(app);
		synthSounds = new ArrayList<SynthSound>();
	}

	// Updates all currently playing sounds
	public void update()
	{
		for (int i = 0; i < synthSounds.size(); i++)
		{
			synthSounds.get(i).update();
			if (synthSounds.get(i).ticksRemaining <= 0)
				synthSounds.remove(i);
		}
	}

	// Creates a new brown noise sound
	public void playExplosionSound(float duration)
	{
		synthSounds.add(
			new ExplosionSound(
				app,
				int(FRAMERATE * duration)));

		if (lowPass != null)
			synthSounds.get(synthSounds.size()-1).applyLowPass(lowPass);
	}

	// Creates a new saw wave sound
	public void playBulletSound(float duration, float frequency)
	{
		synthSounds.add(
			new BulletSound(
				app,
				int(FRAMERATE * duration),
				frequency));

		if (lowPass != null)
			synthSounds.get(synthSounds.size()-1).applyLowPass(lowPass);
	}

	// Applys a lowpass filter to all current and future sounds
	public void applyLowPassFilter()
	{
		lowPass = new LowPass(app);
		for (SynthSound synthSound : synthSounds)
			synthSound.applyLowPass(lowPass);
	}

	// Replaces current sound file being played
	public void playSoundFile(SoundFile soundPlayer)
	{
		if (currentPlayer != null)
			currentPlayer.stop();
		currentPlayer = soundPlayer;
		currentPlayer.play();
	}

	// Stops all currently playing sounds
	public void close()
	{
		for (SynthSound synthSound : synthSounds)
			synthSound.close();
		currentPlayer.stop();
	}
}

// Superclass for all generated sounds with fadeout durations
public abstract class SynthSound
{
	public int duration;
	public int ticksRemaining;

	public SynthSound(int duration)
	{
		this.duration = duration;
		this.ticksRemaining = duration;
	}

	public void update()
	{
		if (--ticksRemaining > 0) setAmp(SOUND_VOLUME * (float)ticksRemaining / (float)duration);
		else close();
	}

	protected abstract void setAmp(float amp);
	protected abstract void applyLowPass(LowPass lowPass);
	protected abstract void close();
}

// A brief brown noise for explosions
public class ExplosionSound extends SynthSound
{
	private BrownNoise noise;

	public ExplosionSound(PApplet app, int duration)
	{
		super(duration);
		this.noise = new BrownNoise(app);
		this.noise.play(SOUND_VOLUME);
	}

	public void setAmp(float amp) { noise.amp(amp); }
	public void applyLowPass(LowPass lowPass) { lowPass.process(noise, GAMEOVER_LOWPASS_THRESHOLD); }
	public void close() { noise.stop(); }
}

// A brief saw wave for bullets
public class BulletSound extends SynthSound
{
	private SawOsc oscillator;

	public BulletSound(PApplet app, int duration, float frequency)
	{
		super(duration);
		this.oscillator = new SawOsc(app);
		this.oscillator.play(frequency, SOUND_VOLUME);
	}

	public void setAmp(float amp) { oscillator.amp(amp); }
	public void applyLowPass(LowPass lowPass) { lowPass.process(oscillator, GAMEOVER_LOWPASS_THRESHOLD); }
	public void close() { oscillator.stop(); }
}


class WaveAlien extends Alien
{
  private final float SPEED = 0.25f;
  private final float WAVE_FREQUENCY = 0.05f;
  private final float WAVE_AMPLITUDE = 100f;
  
  private int time;
  private float startingXPosition;
  
  public WaveAlien(PVector position, PImage aliveTexture, PImage deadTexture)
  {
    this.position = position;
    this.aliveTexture = aliveTexture;
    this.deadTexture = deadTexture;
    currentTexture = this.aliveTexture;
    startingXPosition = this.position.x;
    health = 3;
    time = 0;
  }
  
  @Override
  public void update()
  {
    if (isAlive && velocity.mag() == 0)
    {
      time++;
      position.x = startingXPosition + sin(WAVE_FREQUENCY * time) * WAVE_AMPLITUDE;
      position.y += SPEED;
    }
  }
};

final PVector SCREEN_SIZE = new PVector(640, 480);
final int ALIEN_COUNT = 640 / 40;

PImage textureNormalAlien;
PImage textureWaveAlien;
PImage textureDropAlien;
PImage textureExplosion;
Alien[] aliens;

void settings()
{
  size(int(SCREEN_SIZE.x), int(SCREEN_SIZE.y));
}

void setup()
{
  textureNormalAlien = loadImage("normalAlien.png");
  textureWaveAlien = loadImage("waveAlien.png");
  textureDropAlien = loadImage("dropAlien.png");
  textureExplosion = loadImage("exploding.png");
  
  surface.setTitle("Invaders");
  surface.setIcon(textureNormalAlien);
  aliens = new Alien[ALIEN_COUNT];
  initAliens(aliens);
}

void initAliens(Alien[] alienArray)
{
  for (int index = 0; index < alienArray.length; index++)
  {
    int newAlienType = int(random(3));
    switch (newAlienType)
    {
      case 0:
        alienArray[index] = new NormalAlien(new PVector(index * 40 + 25, 20), textureNormalAlien, textureExplosion);
        break;
      
      case 1:
        alienArray[index] = new WaveAlien(new PVector(index * 40 + 25, 20), textureWaveAlien, textureExplosion);
        break;
      
      case 2:
        alienArray[index] = new DropAlien(new PVector(index * 40 + 25, 20), textureDropAlien, textureExplosion);
        break;
      
      default:
        break;
    }
  }
}

void draw()
{
  background(0);
  for (int index = 0; index < aliens.length; index++)
  {
    aliens[index].update();
    aliens[index].draw();
  }
}

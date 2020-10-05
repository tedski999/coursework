final PVector SCREEN_SIZE = new PVector(640, 480);
final int FRAMERATE = 60;
final int ALIEN_COUNT = 10;
final int PLAYER_MARGIN = 40;
final float POWERUP_CHANCE = 0.5f;
final float BULLET_SPEED_NORMAL = 5;
final float BULLET_SPEED_POWERUP = 10;
final float BULLET_SPLIT_AMOUNT_POWERUP = 1.5f;
final int BULLET_RAPID_COOLDOWN_POWERUP = FRAMERATE / 5;

/* enum Powerup { speed, split, rapid }; // Enums not supported in Processing 3.5.4 */
final int NUMER_OF_POWERUPS = 3;
final int POWERUP_SPEED = 0;
final int POWERUP_SPLIT = 1;
final int POWERUP_RAPID = 2;
final int POWERUP_DURATION = FRAMERATE * 10;

PImage texturePlayer;
PImage textureNormalAlien;
PImage textureWaveAlien;
PImage textureDropAlien;
PImage textureExplosion;
PImage[] powerupTextures;
PImage textureSplitPowerup;
PImage textureRapidPowerup;

Player player;
Alien[] aliens;
ArrayList<Bullet> bullets;
ArrayList<Powerup> powerups;
boolean mouseDown;
int fireCooldown;
int speedPowerupTimeRemaining;
int splitPowerupTimeRemaining;
int rapidPowerupTimeRemaining;

void settings()
{
  size(int(SCREEN_SIZE.x), int(SCREEN_SIZE.y));
}

void setup()
{
  imageMode(CENTER);
  textSize(1);
  textAlign(LEFT);
  textFont(loadFont("Arial-Black-12.vlw"));
  
  texturePlayer = loadImage("player.png");
  textureNormalAlien = loadImage("normalAlien.png");
  textureWaveAlien = loadImage("waveAlien.png");
  textureDropAlien = loadImage("dropAlien.png");
  textureExplosion = loadImage("exploding.png");
  
  powerupTextures = new PImage[NUMER_OF_POWERUPS];
  powerupTextures[POWERUP_SPEED] = loadImage("speedPowerup.png");
  powerupTextures[POWERUP_SPLIT] = loadImage("splitPowerup.png");
  powerupTextures[POWERUP_RAPID] = loadImage("rapidPowerup.png");
  
  frameRate(FRAMERATE);
  surface.setTitle("Invaders");
  surface.setIcon(textureNormalAlien);
  
  player = new Player(new PVector(0, SCREEN_SIZE.y - PLAYER_MARGIN), texturePlayer, textureExplosion);
  bullets = new ArrayList<Bullet>();
  powerups = new ArrayList<Powerup>();
  aliens = new Alien[ALIEN_COUNT];
  initAliens(aliens);
  
  fireCooldown = 0;
  speedPowerupTimeRemaining = 0;
  splitPowerupTimeRemaining = 0;
  rapidPowerupTimeRemaining = 0;
}

void initAliens(Alien[] alienArray)
{
  for (int index = 0; index < alienArray.length; index++)
  {
    int newAlienType = int(random(NUMER_OF_POWERUPS));
    switch (newAlienType)
    {
      case POWERUP_SPEED:
        alienArray[index] = new NormalAlien(new PVector(index * 50 + 25, 20), textureNormalAlien, textureExplosion);
        break;
      
      case POWERUP_SPLIT:
        alienArray[index] = new WaveAlien(new PVector(index * 50 + 25, 20), textureWaveAlien, textureExplosion);
        break;
      
      case POWERUP_RAPID:
        alienArray[index] = new DropAlien(new PVector(index * 50 + 25, 20), textureDropAlien, textureExplosion);
        break;
      
      default:
        break;
    }
  }
}

void draw()
{
  background(0);
  
  if (fireCooldown <= 0 && mouseDown)
    fireBullet();
  
  updatePowerupStats();
  player.update();
  for (int index = 0; index < bullets.size(); index++)
  {
    bullets.get(index).update();
    bullets.get(index).checkForCollisionWithAliens(aliens);
    if (bullets.get(index).isOutOfBounds())
      bullets.remove(index);
  }
  for (int index = 0; index < powerups.size(); index++)
  {
    powerups.get(index).update();
    if (powerups.get(index).checkForCollisionWithPlayer(player) ||
        powerups.get(index).position.y > SCREEN_SIZE.y)
      powerups.remove(index);
  }
  for (Alien alien : aliens)
  {
    alien.update();
    alien.doCollisions();
    //if (alien.isOutOfBounds())
    //  alien.velocity
  }
  
  player.draw();
  for (Bullet bullet : bullets)
    bullet.draw();
  for (Powerup powerup : powerups)
    powerup.draw();
  for (Alien alien : aliens)
    alien.draw();
    
  drawGUI();
}

void drawGUI()
{
  fill(0, 255, 0);
  if (speedPowerupTimeRemaining > 0) text("Bullet Speedup: " + speedPowerupTimeRemaining / FRAMERATE, 0, 10);
  if (splitPowerupTimeRemaining > 0) text("Bullet Split: " + splitPowerupTimeRemaining / FRAMERATE, 0, 25); 
  if (rapidPowerupTimeRemaining > 0) text("Rapid Fire: " + rapidPowerupTimeRemaining / FRAMERATE, 0, 40);

}

void fireBullet()
{
  fireCooldown = BULLET_RAPID_COOLDOWN_POWERUP;
  float ballSpeed = (speedPowerupTimeRemaining <= 0) ? BULLET_SPEED_NORMAL : BULLET_SPEED_POWERUP;
  if (splitPowerupTimeRemaining <= 0)
  {
    bullets.add(new Bullet(player.position.copy(), new PVector(0, -ballSpeed)));
  }
  else
  {
    bullets.add(new Bullet(player.position.copy(), new PVector(-BULLET_SPLIT_AMOUNT_POWERUP, -ballSpeed)));
    bullets.add(new Bullet(player.position.copy(), new PVector(BULLET_SPLIT_AMOUNT_POWERUP, -ballSpeed)));
  }
}

void spawnPowerup(PVector position)
{
  int randomType = int(random(3));
  PVector randomVelocity = new PVector(random(-2f, 2f), 2);
  powerups.add(new Powerup(position, randomVelocity, powerupTextures[randomType], randomType));
}

void updatePowerupStats()
{
  if (speedPowerupTimeRemaining > 0)
    speedPowerupTimeRemaining--;
    
  if (splitPowerupTimeRemaining > 0)
    splitPowerupTimeRemaining--;
    
  if (rapidPowerupTimeRemaining > 0)
  {
    rapidPowerupTimeRemaining--;
    fireCooldown--;
  }
}

void mousePressed()
{
  mouseDown = true;
}

void mouseReleased()
{
  fireCooldown = 0;
  mouseDown = false;
}

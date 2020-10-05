import processing.sound.*;

//=========================================
// GAME CONSTANTS

final boolean	DRAW_DEBUG_INFO = true;
final int		DEBUG_TEXT_SCALE = 10;
final int		GUI_TEXT_SCALE = 20;

// Window constants
final PVector	SCREEN_SIZE = new PVector(800, 600);
final int		FRAMERATE = 60;
final String	WINDOW_TITLE = "Invaders";

// Player constants
final PVector	PLAYER_STARTING_POSITION = new PVector(SCREEN_SIZE.x / 2, SCREEN_SIZE.y - 40);

// Bullet constants
final PVector	BULLET_SIZE = new PVector(5, 5);
final color		BULLET_COLOR = color(255, 255, 255);
final float		BULLET_SPEED = 5f;

// Alien constants
final int		ALIEN_COUNT = 18;
final int		ALIEN_SPACING = 40;
final float		ALIEN_SPEED = 2f;
final int		ALIEN_EXPLOSION_DURATION = int(0.75f * FRAMERATE);
final float		ALIEN_DROP_BOMB_CHANCE = 0.002f;

// Bomb constants
final PVector	BOMB_SIZE = new PVector(10, 10);
final color		BOMB_COLOR = color(255, 0, 0);
final float		BOMB_SPEED = 2f;

// Shield constants
final int		SHIELD_COUNT = 3;
final int		SHIELD_SPACING = 250;
final PVector	SHIELD_SIZE = new PVector(15, 3);
final PVector	SHIELD_STARTING_MARGIN = new PVector(75, SCREEN_SIZE.y - 150);
final PVector	SHIELD_BLOCK_SIZE = new PVector(10, 10);
final color		SHIELD_BLOCK_COLOR = color(0, 255, 0);

// Sounds constants
final float		MUSIC_VOLUME = 1f;
final float		SOUND_VOLUME = 0.3f;
final float		PLAYER_SHOT_SOUND_FREQ = 200f;
final float		ALIEN_SHOT_SOUND_FREQ = 150f;
final float		GAMEOVER_LOWPASS_THRESHOLD = 800f;

//=========================================
// GLOBAL VARIABLES

AudioEngine audioEngine;

// Resources
PFont debugFont;
PFont guiFont;
PImage playerTexture;
PImage alienTexture;
PImage explosionTexture;
SoundFile losingTunePlayer;
SoundFile winningTunePlayer;
SoundFile musicPlayer;

// Gameobjects
Player player;
ArrayList<Bullet> bullets;
ArrayList<Alien> aliens;
ArrayList<ShieldBlock> shieldBlocks;

// Game state
boolean gameIsRunning;
boolean playerHasWon;


//=========================================
// GAME FUNCTIONS

// Game entrypoint
void settings()
{
	size(int(SCREEN_SIZE.x), int(SCREEN_SIZE.y));
}

// Called after the window is created in settings()
void setup()
{
	// Setup drawing modes
	imageMode(CENTER);
	rectMode(CENTER);
	noStroke();

	// Load loading font
	guiFont = loadFont("Arial-Black-12.vlw");
	textFont(guiFont);

	// Draw loading screen
	background(0);
	textSize(DEBUG_TEXT_SCALE);
	textAlign(LEFT);
	text("© Ted Johnon - Programming", 0, DEBUG_TEXT_SCALE * 1);
	text("© Tomohiro Nishikado - Music", 0, DEBUG_TEXT_SCALE * 2);
	textSize(GUI_TEXT_SCALE);
	textAlign(CENTER);
	text("Loading...", SCREEN_SIZE.x / 2, SCREEN_SIZE.y - 100);

	// Load resources
	debugFont = loadFont("LucidaConsole-11.vlw");
	playerTexture = loadImage("player.png");
	alienTexture = loadImage("alien.png");
	explosionTexture = loadImage("explosion.png");
	winningTunePlayer = new SoundFile(this, "winningTune.wav");
	losingTunePlayer = new SoundFile(this, "losingTune.wav");
	musicPlayer = new SoundFile(this, "music.mp3");

	// Set sound volumes
	winningTunePlayer.amp(SOUND_VOLUME);
	losingTunePlayer.amp(SOUND_VOLUME);
	musicPlayer.amp(MUSIC_VOLUME);

	// Customize window
	frameRate(FRAMERATE);
	surface.setTitle(WINDOW_TITLE);
	surface.setIcon(alienTexture);

	newGame();
}

// Initializes all variables for a new game
void newGame()
{
	// Start a new audio engine
	if (audioEngine != null) audioEngine.close();
	audioEngine = new AudioEngine(this);
	audioEngine.playSoundFile(musicPlayer);

	// Instantiate gameobjects
	player = new Player(PLAYER_STARTING_POSITION, playerTexture, explosionTexture);
	bullets = new ArrayList<Bullet>();
	aliens = new ArrayList<Alien>();
	shieldBlocks = new ArrayList<ShieldBlock>();
	initAliens();
	initShields();

	// Start game
	gameIsRunning = true;
}

// Adds aliens to the alien arraylist
void initAliens()
{
	for (int i = 0; i < ALIEN_COUNT; i++)
	{
		aliens.add(
			new Alien(
				new PVector(
					alienTexture.width + i * ALIEN_SPACING,
					alienTexture.height),
				alienTexture,
				explosionTexture));
	}
}

// Adds shields to the shields array
void initShields()
{
	for (int i = 0; i < SHIELD_COUNT; i++)
	{
		PVector currentShieldPosition = new PVector(SHIELD_STARTING_MARGIN.x + i * SHIELD_SPACING, SHIELD_STARTING_MARGIN.y);
		for (int yOffset = 0; yOffset < SHIELD_SIZE.y; yOffset++)
		{
			for (int xOffset = 0; xOffset < SHIELD_SIZE.x; xOffset++)
			{
				shieldBlocks.add(
					new ShieldBlock(
						new PVector(
							currentShieldPosition.x + xOffset * SHIELD_BLOCK_SIZE.x,
							currentShieldPosition.y + yOffset * SHIELD_BLOCK_SIZE.y)));
			}
		}
	}
}

// The main game loop, called every 1/FRAMERATE milliseconds
void draw()
{
	background(0);
	audioEngine.update();

	// Update game state
	if (gameIsRunning)
	{
		if (aliens.size() == 0)
		{
			gameIsRunning = false;
			playerHasWon = true;
			audioEngine.playSoundFile(winningTunePlayer);
			audioEngine.applyLowPassFilter();
		}
		else if (!player.isAlive)
		{
			gameIsRunning = false;
			playerHasWon = false;
			audioEngine.playSoundFile(losingTunePlayer);
			audioEngine.applyLowPassFilter();
		}
	}

	// Update Player
	player.update();
	player.draw();

	// Update Bullets
	for (int i = 0; i < bullets.size(); i++)
	{
		Bullet bullet = bullets.get(i);
		bullet.update();
		bullet.doCollisions(aliens);
		bullet.draw();
		if (bullet.isOffscreen())
			bullets.remove(i);
	}

	// Update Aliens
	for (int i = 0; i < aliens.size(); i++)
	{
		Alien alien = aliens.get(i);
		alien.update();
		alien.doCollisions(player);
		alien.draw();
		if (!alien.isAlive && alien.getDeathTimeRemaining() <= 0)
			aliens.remove(i);
	}

	// Update shields
	for (int i = 0; i < shieldBlocks.size(); i++)
	{
		ShieldBlock shieldBlock = shieldBlocks.get(i);
		if (shieldBlock.doCollisions(bullets, aliens))
			shieldBlocks.remove(i);
		else
			shieldBlock.draw();
	}

	drawGUI();
}

// Draws all the text onto the screen
void drawGUI()
{
	if (DRAW_DEBUG_INFO)
	{
		fill(255);
		textAlign(LEFT);
		textFont(debugFont);
		textSize(DEBUG_TEXT_SCALE);
		text("Bullet count: " + bullets.size(), 0, DEBUG_TEXT_SCALE * 1);
		text("Alien count:  " + aliens.size(), 0, DEBUG_TEXT_SCALE * 2);
		text("Shield count: " + shieldBlocks.size(), 0, DEBUG_TEXT_SCALE * 3);
		text("Sound volume: " + SOUND_VOLUME, 0, DEBUG_TEXT_SCALE * 4);
		text("Music volume: " + MUSIC_VOLUME, 0, DEBUG_TEXT_SCALE * 5);
	}

	if (!gameIsRunning)
	{
		textAlign(CENTER);
		textFont(guiFont);
		fill(0, 255, 0);
		textSize(GUI_TEXT_SCALE / 2);
		text("Press 'R' to play again", SCREEN_SIZE.x / 2, SCREEN_SIZE.y / 2 + GUI_TEXT_SCALE);
		textSize(GUI_TEXT_SCALE);

		if (playerHasWon)
		{
			fill(255, 255, 0);
			text("You Win!", SCREEN_SIZE.x / 2, SCREEN_SIZE.y / 2);
		}
		else
		{
			fill(255, 0, 0);
			text("Game Over!", SCREEN_SIZE.x / 2, SCREEN_SIZE.y / 2);
		}
	}
}

// Utility function for AABB collisions
boolean isColliding(PVector aPosition, PVector aSize, PVector bPosition, PVector bSize)
{
	return (
		aPosition.x - (aSize.x / 2) < bPosition.x + (bSize.x / 2) &&
		aPosition.x + (aSize.x / 2) > bPosition.x - (bSize.x / 2) &&
		aPosition.y - (aSize.y / 2) < bPosition.y + (bSize.y / 2) &&
		aPosition.y + (aSize.y / 2) > bPosition.y - (bSize.y / 2));
}

// Called when the mouse is clicked
void mousePressed()
{
	if (player.isAlive)
	{
		bullets.add(
			new Bullet(
				player.getPosition().copy(),
				new PVector(0, -BULLET_SPEED)));
		audioEngine.playBulletSound(0.3f, PLAYER_SHOT_SOUND_FREQ);
	}
}

// Called after every keypress
void keyPressed()
{
	if (!gameIsRunning && key == 'r')
		newGame();
}
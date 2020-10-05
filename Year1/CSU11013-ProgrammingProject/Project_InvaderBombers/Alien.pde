
public class Alien
{
	public boolean isAlive;
	private PVector position;
	private int currentState;
	private int nextState;
	private Bomb bomb;
	private float targetYPosition;
	private int deathTimeRemaining;

	private PImage currentTexture;
	private PImage normalTexture;
	private PImage deadTexture;

	private final int STATE_LEFT = 0;
	private final int STATE_RIGHT = 1;
	private final int STATE_DOWN = 2;

	public Alien(PVector position, PImage normalTexture, PImage deadTexture)
	{
		isAlive = true;
		this.position = position;
		this.normalTexture = normalTexture;
		this.deadTexture = deadTexture;
		currentState = STATE_LEFT;
		currentTexture = this.normalTexture;
		deathTimeRemaining = ALIEN_EXPLOSION_DURATION;
		bomb = null;
	}

	public void update()
	{
		if (isAlive)
		{
			// Randomly spawn a single bomb
			if (bomb == null && random(1 / ALIEN_DROP_BOMB_CHANCE) <= 1)
			{
				bomb = new Bomb(position.copy(), new PVector(0, BOMB_SPEED));
				audioEngine.playBulletSound(0.2f, ALIEN_SHOT_SOUND_FREQ);
			}

			// Update bomb
			if (bomb != null)
			{
				bomb.update();
				if (bomb.isOffscreen())
					destroyBomb();
			}

			// Perform alien movement behaviour
			switch (currentState)
			{
				case (STATE_LEFT):
					position.x += ALIEN_SPEED;
					if (position.x + (currentTexture.width / 2) >= SCREEN_SIZE.x)
					{
						currentState = STATE_DOWN;
						nextState = STATE_RIGHT;
						targetYPosition = position.y + currentTexture.height;
					}
					break;

				case (STATE_RIGHT):
					position.x -= ALIEN_SPEED;
					if (position.x - (currentTexture.width / 2) <= 0)
					{
						currentState = STATE_DOWN;
						nextState = STATE_LEFT;
						targetYPosition = position.y + currentTexture.height;
					}
					break;

				case (STATE_DOWN):
					position.y += ALIEN_SPEED;
					if (position.y >= targetYPosition)
						currentState = nextState;
					break;

				default:
					break;
			}
		}
		else
			deathTimeRemaining--;
	}

	public void draw()
	{
		if (isAlive && bomb != null) bomb.draw();
		image(currentTexture, position.x, position.y);
	}

	public void doCollisions(Player player)
	{
		if (isColliding(position, getSize(), player.getPosition(), player.getSize()))
			player.explode();

		if (bomb != null)
			if (bomb.doCollisions(player))
				destroyBomb();
	}

	public PVector getPosition()
	{
		return position;
	}

	public PVector getSize()
	{
		return new PVector(currentTexture.width, currentTexture.height);
	}

	public int getDeathTimeRemaining()
	{
		return deathTimeRemaining;
	}

	public Bomb getBomb()
	{
		return bomb;
	}

	public void destroyBomb()
	{
		bomb = null;
	}

	public void explode()
	{
		isAlive = false;
		currentTexture = deadTexture;
		audioEngine.playExplosionSound(0.4f);
	}
}

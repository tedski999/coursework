
public class Player
{
	public boolean isAlive;
	private PVector position;
	private PImage currentTexture;
	private PImage normalTexture;
	private PImage deadTexture;

	public Player(PVector position, PImage normalTexture, PImage deadTexture)
	{
		isAlive = true;
		this.position = position;
		this.normalTexture = normalTexture;
		this.deadTexture = deadTexture;
		currentTexture = this.normalTexture;
	}

	public void update()
	{
		if (isAlive)
		{
			if (mouseX + (currentTexture.width / 2) > SCREEN_SIZE.x)
				position.x = SCREEN_SIZE.x - (currentTexture.width / 2);
			else if (mouseX - (currentTexture.width / 2) < 0)
				position.x = currentTexture.width / 2;
			else
				position.x = mouseX;
		}
	}

	public void draw()
	{
		image(currentTexture, position.x, position.y);
	}

	public PVector getPosition()
	{
		return position;
	}

	public PVector getSize()
	{
		return new PVector(currentTexture.width, currentTexture.height);
	}

	public void explode()
	{
		isAlive = false;
		currentTexture = deadTexture;
		audioEngine.playExplosionSound(1f);
	}
}

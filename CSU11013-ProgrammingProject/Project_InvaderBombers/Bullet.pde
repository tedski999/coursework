
public class Bullet
{
	private PVector position;
	private PVector velocity;

	public Bullet(PVector position, PVector velocity)
	{
		this.position = position;
		this.velocity = velocity;
	}

	public void update()
	{
		position.add(velocity);
	}

	public void draw()
	{
		fill(BULLET_COLOR);
		rect(position.x, position.y, BULLET_SIZE.x, BULLET_SIZE.y);
	}

	public void doCollisions(ArrayList<Alien> aliens)
	{
		for (Alien alien : aliens)
		{
			if (alien.isAlive)
				if (isColliding(position, BULLET_SIZE, alien.getPosition(), alien.getSize()))
					alien.explode();
		}
	}

	public PVector getPosition()
	{
		return position;
	}

	public boolean isOffscreen()
	{
		return (
			position.x - (BULLET_SIZE.x / 2) < 0 ||
			position.x + (BULLET_SIZE.x / 2) > SCREEN_SIZE.x ||
			position.y - (BULLET_SIZE.y / 2) < 0 ||
			position.y + (BULLET_SIZE.y / 2) > SCREEN_SIZE.y);
	}
}

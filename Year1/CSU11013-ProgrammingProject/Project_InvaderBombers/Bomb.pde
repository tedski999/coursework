
public class Bomb
{
	public boolean hasExploded;
	private PVector position;
	private PVector velocity;

	public Bomb(PVector position, PVector velocity)
	{
		hasExploded = false;
		this.position = position;
		this.velocity = velocity;
	}

	public void update()
	{
		position.add(velocity);
	}

	public void draw()
	{
		fill(BOMB_COLOR);
		rect(position.x, position.y, BOMB_SIZE.x, BOMB_SIZE.y);
	}

	public boolean doCollisions(Player player)
	{
		if (player.isAlive)
		{
			if (isColliding(position, BOMB_SIZE, player.getPosition(), player.getSize()))
			{
				player.explode();
				return true;
			}
		}

		return false;
	}

	public PVector getPosition()
	{
		return position;
	}

	public boolean isOffscreen()
	{
		return (
			position.x - (BOMB_SIZE.x / 2) < 0 ||
			position.x + (BOMB_SIZE.x / 2) > SCREEN_SIZE.x ||
			position.y - (BOMB_SIZE.y / 2) < 0 ||
			position.y + (BOMB_SIZE.y / 2) > SCREEN_SIZE.y);
	}
}

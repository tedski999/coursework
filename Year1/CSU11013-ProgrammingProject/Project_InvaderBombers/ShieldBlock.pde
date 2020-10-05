
public class ShieldBlock
{
	private PVector position;

	public ShieldBlock(PVector position)
	{
		this.position = position;
	}

	public boolean doCollisions(ArrayList<Bullet> bullets, ArrayList<Alien> aliens)
	{
		for (int i = 0; i < bullets.size(); i++)
		{
			Bullet bullet = bullets.get(i);
			if (isColliding(position, SHIELD_BLOCK_SIZE, bullet.getPosition(), BULLET_SIZE))
			{
				audioEngine.playExplosionSound(0.1f);
				bullets.remove(i);
				return true;
			}
		}

		for (Alien alien : aliens)
		{
			if (alien.getBomb() != null)
			{
				if (isColliding(position, SHIELD_BLOCK_SIZE, alien.getBomb().getPosition(), BOMB_SIZE))
				{
					audioEngine.playExplosionSound(0.1f);
					alien.destroyBomb();
					return true;
				}
			}

			if (isColliding(position, SHIELD_BLOCK_SIZE, alien.getPosition(), alien.getSize()))
				return true;
		}

		return false;
	}

	public void draw()
	{
		fill(SHIELD_BLOCK_COLOR);
		rect(position.x, position.y, SHIELD_BLOCK_SIZE.x, SHIELD_BLOCK_SIZE.y);
	}
}

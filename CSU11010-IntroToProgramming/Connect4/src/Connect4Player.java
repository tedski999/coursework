public abstract class Connect4Player
{
	private char discSymbol;

	public Connect4Player(char diskSymbol)
	{
		setDiskSymbol(diskSymbol);
	}

	public void setDiskSymbol(char discSymbol)
	{
		this.discSymbol = discSymbol;
	}

	public char getDiskSymbol()
	{
		return discSymbol;
	}

	// Overridden by subclasses
	public abstract int selectColumn(Connect4Grid grid);
}

public interface Connect4Grid
{
	void emptyGrid();
	String toString();
	boolean isValidColumn(int column);
	boolean isColumnFull(int column);
	void dropPiece(Connect4Player player, int column);
	boolean didLastPieceConnect4();
	boolean isGridFull();
}

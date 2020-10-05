
public class VectorFunctions
{
	public static void main(String[] args)
	{
		double[] vector1 = { 12, 5 };
		double[] vector2 = { 3, 4 };

		double magnitude = getMagnitudeOfVector(vector1);
		System.out.println("Magnitude of (12,5): " + magnitude);

		double dotProduct = computeDotProductOfVectors(vector1, vector2);
		System.out.println("Dot product of (5,6) and (2,5): " + dotProduct);

		double[] sum = addVectors(vector1, vector2);
		System.out.println("Sum of (5,6) and (2,5): (" + sum[0] + "," + sum[1] + ")");

		normiliseVector(vector1);
		System.out.println("Normilised (12,5): (" + vector1[0] + "," + vector1[1] + ")");
	}

	public static double getMagnitudeOfVector(double[] vector)
	{
		if (vector == null)
			return 0;

		double magnitude = 0;
		for (int index = 0; index < vector.length; index++)
			magnitude += Math.pow(vector[index], 2);
		magnitude = Math.sqrt(magnitude);
		return magnitude;
	}

	public static void normiliseVector(double[] vector)
	{
		if (vector == null)
			return;

		double magnitude = getMagnitudeOfVector(vector);
		for (int index = 0; index < vector.length; index++)
			vector[index] /= magnitude;
	}

	public static double computeDotProductOfVectors(double[] vector1, double[] vector2)
	{
		if (vector1 == null || vector2 == null)
			throw new RuntimeException("Cannot compute a dot product involving null vectors!");

		if (vector1.length != vector2.length)
			throw new RuntimeException("Cannot compute dot product of vectors with different dimensions!");

		double result = 0;
		for (int index = 0; index < vector1.length; index++)
			result += vector1[index] * vector2[index];
		return result;
	}

	public static double[] addVectors(double[] vector1, double[] vector2)
	{
		if (vector1 == null || vector2 == null)
			throw new RuntimeException("Cannot compute addition involving null vectors!");

		if (vector1.length != vector2.length)
			throw new RuntimeException("Cannot compute addition of vectors with different dimensions!");

		double[] result = new double[vector1.length];
		for (int index = 0; index < result.length; index++)
			result[index] = vector1[index] + vector2[index];
		return result;
	}
}

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.util.Arrays;

//-------------------------------------------------------------------------
/**
 *  Test class for Collinear.java
 *
 *  @author  
 *  @version 18/09/18 12:21:26
 */
@RunWith(JUnit4.class)
public class CollinearTest
{
    //~ Constructor ........................................................
    @Test
    public void testConstructor()
    {
      new Collinear();
    }

    //~ Public Methods ........................................................

    // ----------------------------------------------------------
    /**
     * Check that the two methods work for empty arrays
     */
    @Test
    public void testEmpty()
    {
        int expectedResult = 0;

        assertEquals("countCollinear failed with 3 empty arrays",       expectedResult, Collinear.countCollinear(new int[0], new int[0], new int[0]));
        assertEquals("countCollinearFast failed with 3 empty arrays", expectedResult, Collinear.countCollinearFast(new int[0], new int[0], new int[0]));
    }

    // ----------------------------------------------------------
    /**
     * Check for no false positives in a single-element array
     */
    @Test
    public void testSingleFalse()
    {
        int[] a3 = { 15 };
        int[] a2 = { 5 };
        int[] a1 = { 10 };

        int expectedResult = 0;

        assertEquals("countCollinear({10}, {5}, {15})",       expectedResult, Collinear.countCollinear(a1, a2, a3) );
        assertEquals("countCollinearFast({10}, {5}, {15})", expectedResult, Collinear.countCollinearFast(a1, a2, a3) );
    }

    // ----------------------------------------------------------
    /**
     * Check for no false positives in a single-element array
     */
    @Test
    public void testSingleTrue()
    {
        int[] a3 = { 15, 5 };       int[] a2 = { 5 };       int[] a1 = { 10, 15, 5 };

        int expectedResult = 1;

        assertEquals("countCollinear(" + Arrays.toString(a1) + "," + Arrays.toString(a2) + "," + Arrays.toString(a3) + ")",     expectedResult, Collinear.countCollinear(a1, a2, a3));
        assertEquals("countCollinearFast(" + Arrays.toString(a1) + "," + Arrays.toString(a2) + "," + Arrays.toString(a3) + ")", expectedResult, Collinear.countCollinearFast(a1, a2, a3));
    }

    // ----------------------------------------------------------
    /**
     * Check for no false positives in a multi-element array
     */
    @Test
    public void testMultipleFalse()
    {
        int[] a3 = { 4787, 774, 5871, 4192, 8131, 1539, 1335, 7976, 4193, 2113, 8530, 8856, 9525, 2913, 4627, 9321, 125, 5614, 3180, 5618, 6123, 4721, 4407, 8348 };
        int[] a2 = { 9162, 8116, 5709, 3086, 6800, 3367, 6622, 4708, 533, 7189, 981, 22, 3372, 2397, 8100, 2614, 6245, 6812, 7062, 9604, 9697, 9761, 1621, 7088, 4957, 3853 };
        int[] a1 = { 3977, 2053, 515, 6112, 2660, 4771, 144, 1737, 6042, 3357, 4200, 9307, 1100, 7403, 8124, 8815, 6626, 7006, 4683, 1612, 5316, 5907, 6754, 572, 5314 };

        int expectedResult = 0;

        assertEquals("countCollinear(" + Arrays.toString(a1) + "," + Arrays.toString(a2) + "," + Arrays.toString(a3) + ")",     expectedResult, Collinear.countCollinear(a1, a2, a3));
        assertEquals("countCollinearFast(" + Arrays.toString(a1) + "," + Arrays.toString(a2) + "," + Arrays.toString(a3) + ")", expectedResult, Collinear.countCollinearFast(a1, a2, a3));
    }

    // ----------------------------------------------------------
    /**
     * Check for no false positives in a multi-element array
     */
    @Test
    public void testMultipleTrue()
    {
        int[] a3 = { 4787, 3, 5871, 4192, 8131, 1539, 1335, 36, 4193, 2113, 8530, 8856, 9525, 2913, 6, 9321, 125, 5614, 3180, 5618, 6123, 4721, 4407, 8348 };
        int[] a2 = { 9162, 2, 5709, 24, 6800, 3367, 6622, 4708, 4, 7189, 981, 22, 3372, 2397, 8100, 2614, 6245, 6812, 7062, 9604, 9697, 9761, 1621, 7088, 4957, 3853 };
        int[] a1 = { 3977, 1, 515, 6112, 2, 4771, 144, 1737, 6042, 3357, 4200, 9307, 1100, 7403, 8124, 8815, 6626, 7006, 4683, 12, 5316, 5907, 6754, 572, 5314 };

        int expectedResult = 3;

        assertEquals("countCollinear(" + Arrays.toString(a1) + "," + Arrays.toString(a2) + "," + Arrays.toString(a3) + ")",     expectedResult, Collinear.countCollinear(a1, a2, a3));
        assertEquals("countCollinearFast(" + Arrays.toString(a1) + "," + Arrays.toString(a2) + "," + Arrays.toString(a3) + ")", expectedResult, Collinear.countCollinearFast(a1, a2, a3));
    }
}


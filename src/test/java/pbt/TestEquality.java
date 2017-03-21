package pb;

import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class TestEquality {

   @Test 
   public void testIntAplusB() {
      assertEquals(2 + 3, 3 + 2);
      assertEquals(1 + 5, 5 + 1);
   }

   @Test
   public void testFloatAplusB() {
     assertEquals(.2 + .4, .4 + .2, .001);
     assertEquals(.1 + .3, .3 + .1, .001);
   }

}

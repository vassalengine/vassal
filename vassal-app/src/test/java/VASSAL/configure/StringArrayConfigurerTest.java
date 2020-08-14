package VASSAL.configure;

import javax.swing.JFrame;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.stream.IntStream;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.BlockJUnit4ClassRunner;

@RunWith(BlockJUnit4ClassRunner.class)
public class StringArrayConfigurerTest {

  @Test
  public void compareStringArrayToStringPerformance() {
    final String[] input = IntStream
      .range(0, 1000)
      .mapToObj(value -> "string" + value)
      .toArray(String[]::new);

    // warm up VM
    IntStream
      .range(0, 100000)
      .forEach(value -> {
        final String sOld = StringArrayConfigurer.arrayToStringOld(input);
        final String sNew = StringArrayConfigurer.arrayToString(input);
      });

    // measure old
    final long oldStart = System.currentTimeMillis();
    IntStream
      .range(0, 100000)
      .forEach(value -> {
        final String s = StringArrayConfigurer.arrayToStringOld(input);
      });
    final long oldDuration = System.currentTimeMillis() - oldStart;

    // measure new
    final long newStart = System.currentTimeMillis();
    IntStream
      .range(0, 100000)
      .forEach(value -> {
        final String s = StringArrayConfigurer.arrayToString(input);
      });
    final long newDuration = System.currentTimeMillis() - newStart;

    System.out.println("old method duration: " + oldDuration + "\nnew method duration: " + newDuration);
  }

  @Ignore
  @Test
  public void manualTest() {
    JFrame f = new JFrame();
    final StringArrayConfigurer c = new StringArrayConfigurer(null, "Visible to these players:  ");
    c.addPropertyChangeListener(new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent evt) {
        System.err.println(c.getName() + " = " + c.getValueString());
      }
    });
    c.setValue("Rack,Shack,Benny");
    f.add(c.getControls());
    f.pack();
    f.setVisible(true);
  }
}

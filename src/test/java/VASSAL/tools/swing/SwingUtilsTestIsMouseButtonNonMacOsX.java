package VASSAL.tools.swing;

import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.util.Arrays;
import java.util.Collection;

import org.junit.Rule;
import org.junit.contrib.java.lang.system.ProvideSystemProperty;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class SwingUtilsTestIsMouseButtonNonMacOsX extends AbstractSwingUtilsTestIsMouseButton {

  public SwingUtilsTestIsMouseButtonNonMacOsX(int[] mouseEventParams, boolean[] expectedResult) {
    super(mouseEventParams, expectedResult);
  }

  @Parameterized.Parameters
  public static Collection<Object[]> testData() {
    return Arrays.asList(new Object[][]{
      // left
      { new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON1, 0}, new boolean[] {true, false}},
      { new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON1, 0}, new boolean[] {true, false}},
      { new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1, 0}, new boolean[] {true, false}},
      // left + ctrl
      { new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON1, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}},
      { new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON1, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}},
      { new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}},
      // left movement
      { new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON1_DOWN_MASK}, new boolean[] {true, false}},
      { new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON1_DOWN_MASK}, new boolean[] {true, false}},
      { new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON1_DOWN_MASK}, new boolean[] {true, false}},
      // left movement + ctrl
      { new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON1_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}},
      { new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON1_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}},
      { new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON1_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}},

      // middle
      { new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON2, 0}, new boolean[] {false, false}},
      { new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON2, 0}, new boolean[] {false, false}},
      { new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON2, 0}, new boolean[] {false, false}},
      // middle movement
      { new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON2_DOWN_MASK}, new boolean[] {false, false}},
      { new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON2_DOWN_MASK}, new boolean[] {false, false}},
      { new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON2_DOWN_MASK}, new boolean[] {false, false}},

      // right
      { new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON3, 0}, new boolean[] {false, true}},
      { new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON3, 0}, new boolean[] {false, true}},
      { new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON3, 0}, new boolean[] {false, true}},
      // right + ctrl
      { new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON3, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}},
      { new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON3, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}},
      { new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON3, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}},
      // right movement
      { new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON3_DOWN_MASK}, new boolean[] {false, true}},
      { new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON3_DOWN_MASK}, new boolean[] {false, true}},
      { new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON3_DOWN_MASK}, new boolean[] {false, true}},
      // right movement + ctrl
      { new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON3_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}},
      { new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON3_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}},
      { new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON3_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}},
    });
  }

  @Rule
  public final ProvideSystemProperty osNameProperty =
    new ProvideSystemProperty("os.name", "anythingButMacOsx");

}

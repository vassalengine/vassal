package VASSAL.tools.swing;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.util.Arrays;
import java.util.Collection;

import org.junit.Rule;
import org.junit.Test;
import org.junit.contrib.java.lang.system.ProvideSystemProperty;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class SwingUtilsTestIsMouseButtonNonMacOsX {

  /**
   * index 0: {@link MouseEvent#getID()}
   *   e.g. {@link MouseEvent#MOUSE_PRESSED}
   *
   * index 1: {@link MouseEvent#getButton()}
   *   e.g. {@link MouseEvent#BUTTON1}
   *
   * index 2: {@link MouseEvent#getModifiersEx()}
   *   e.g. {@link InputEvent#BUTTON1_DOWN_MASK}
   */
  private final int[] mouseEventParams;

  /**
   * index 0: result for isMouseLeft
   * index 1: result for isMouseRight
   */
  private final boolean[] expectedResult;

  public SwingUtilsTestIsMouseButtonNonMacOsX(int[] mouseEventParams, boolean[] expectedResult) {
    this.mouseEventParams = mouseEventParams;
    this.expectedResult = expectedResult;
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
  public final ProvideSystemProperty myPropertyHasMyValue =
      new ProvideSystemProperty("os.name", "anythingButMacOsx");

  @Test
  public void testIsLeftMouseButton() {
    // prepare
    final MouseEvent mouseEvent = mock(MouseEvent.class);
    when(mouseEvent.getID()).thenReturn(mouseEventParams[0]);
    when(mouseEvent.getButton()).thenReturn(mouseEventParams[1]);
    when(mouseEvent.getModifiersEx()).thenReturn(mouseEventParams[2]);

    // run
    final boolean result = SwingUtils.isLeftMouseButton(mouseEvent);

    // assert
    assertThat(result, is(expectedResult[0]));
  }

  @Test
  public void testIsRightMouseButton() {
    // prepare
    final MouseEvent mouseEvent = mock(MouseEvent.class);
    when(mouseEvent.getID()).thenReturn(mouseEventParams[0]);
    when(mouseEvent.getButton()).thenReturn(mouseEventParams[1]);
    when(mouseEvent.getModifiersEx()).thenReturn(mouseEventParams[2]);

    // run
    final boolean result = SwingUtils.isRightMouseButton(mouseEvent);

    // assert
    assertThat(result, is(expectedResult[1]));
  }

}

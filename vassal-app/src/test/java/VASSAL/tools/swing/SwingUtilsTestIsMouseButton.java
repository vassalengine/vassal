package VASSAL.tools.swing;

import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.util.stream.Stream;

import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.condition.OS.MAC;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class SwingUtilsTestIsMouseButton {

  @ParameterizedTest
  @MethodSource("macOSXParams")
  @EnabledOnOs({ MAC })
  public void isLeftMouseButtonMacOSX(int[] mouseEventParams,
                                    boolean[] expectedResult) {
    isLeftMouseButton(mouseEventParams, expectedResult);
  }

  @ParameterizedTest
  @MethodSource("macOSXParams")
  @EnabledOnOs({ MAC })
  public void isRightMouseButtonMacOSX(int[] mouseEventParams,
                                     boolean[] expectedResult) {
    isRightMouseButton(mouseEventParams, expectedResult);
  }

  @ParameterizedTest
  @MethodSource("nonMacOSXParams")
  @DisabledOnOs({ MAC })
  public void isLeftMouseButtonNonMacOSX(int[] mouseEventParams,
                                      boolean[] expectedResult) {
    isLeftMouseButton(mouseEventParams, expectedResult);
  }

  @ParameterizedTest
  @MethodSource("nonMacOSXParams")
  @DisabledOnOs({ MAC })
  public void isRightMouseButtonNonMacOSX(int[] mouseEventParams,
                                       boolean[] expectedResult) {
    isRightMouseButton(mouseEventParams, expectedResult);
  }

  private void isLeftMouseButton(int[] mouseEventParams,
                                    boolean[] expectedResult) {
    // prepare
    final MouseEvent mouseEvent = mock(MouseEvent.class);
    when(mouseEvent.getID()).thenReturn(mouseEventParams[0]);
    when(mouseEvent.getButton()).thenReturn(mouseEventParams[1]);
    when(mouseEvent.getModifiersEx()).thenReturn(mouseEventParams[2]);

    // run
    final boolean result = SwingUtils.isMainMouseButtonDown(mouseEvent);

    // assert
    assertThat(result, is(expectedResult[0]));
  }

  private void isRightMouseButton(int[] mouseEventParams,
                                     boolean[] expectedResult) {
    // prepare
    final MouseEvent mouseEvent = mock(MouseEvent.class);
    when(mouseEvent.getID()).thenReturn(mouseEventParams[0]);
    when(mouseEvent.getButton()).thenReturn(mouseEventParams[1]);
    when(mouseEvent.getModifiersEx()).thenReturn(mouseEventParams[2]);

    // run
    final boolean result = SwingUtils.isContextMouseButtonDown(mouseEvent);

    // assert
    assertThat(result, is(expectedResult[1]));
  }

  private static Stream<Arguments> nonMacOSXParams() {
    return Stream.of(
        // left
        Arguments.of(new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON1, 0}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON1, 0}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1, 0}, new boolean[] {true, false}),
        // left + ctrl
        Arguments.of(new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON1, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON1, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}),
        // left movement
        Arguments.of(new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON1_DOWN_MASK}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON1_DOWN_MASK}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON1_DOWN_MASK}, new boolean[] {true, false}),
        // left movement + ctrl
        Arguments.of(new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON1_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON1_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON1_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {true, false}),

        // middle
        Arguments.of(new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON2, 0}, new boolean[] {false, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON2, 0}, new boolean[] {false, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON2, 0}, new boolean[] {false, false}),
        // middle movement
        Arguments.of(new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON2_DOWN_MASK}, new boolean[] {false, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON2_DOWN_MASK}, new boolean[] {false, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON2_DOWN_MASK}, new boolean[] {false, false}),

        // right
        Arguments.of(new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON3, 0}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON3, 0}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON3, 0}, new boolean[] {false, true}),
        // right + ctrl
        Arguments.of(new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON3, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON3, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON3, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        // right movement
        Arguments.of(new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON3_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON3_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON3_DOWN_MASK}, new boolean[] {false, true}),
        // right movement + ctrl
        Arguments.of(new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON3_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON3_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON3_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}));
  }

  private static Stream<Arguments> macOSXParams() {
    return Stream.of(
        // left
        Arguments.of(new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON1, 0}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON1, 0}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1, 0}, new boolean[] {true, false}),
        // left + ctrl -> right button on Mac
        Arguments.of(new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON1, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON1, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        // left movement
        Arguments.of(new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON1_DOWN_MASK}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON1_DOWN_MASK}, new boolean[] {true, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON1_DOWN_MASK}, new boolean[] {true, false}),
        // left movement + ctrl -> right button on Mac
        Arguments.of(new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON1_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON1_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON1_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        // middle
        Arguments.of(new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON2, 0}, new boolean[] {false, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON2, 0}, new boolean[] {false, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON2, 0}, new boolean[] {false, false}),
        // middle movement
        Arguments.of(new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON2_DOWN_MASK}, new boolean[] {false, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON2_DOWN_MASK}, new boolean[] {false, false}),
        Arguments.of(new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON2_DOWN_MASK}, new boolean[] {false, false}),
        // right
        Arguments.of(new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON3, 0}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON3, 0}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON3, 0}, new boolean[] {false, true}),
        // right + ctrl
        Arguments.of(new int[] {MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON3, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON3, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON3, MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        // right movement
        Arguments.of(new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON3_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON3_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON3_DOWN_MASK}, new boolean[] {false, true}),
        // right movement + ctrl
        Arguments.of(new int[] {MouseEvent.MOUSE_ENTERED, 0, InputEvent.BUTTON3_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_EXITED, 0, InputEvent.BUTTON3_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}),
        Arguments.of(new int[] {MouseEvent.MOUSE_DRAGGED, 0, InputEvent.BUTTON3_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK}, new boolean[] {false, true}));
  }

}

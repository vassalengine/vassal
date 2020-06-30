package VASSAL.tools.swing;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;

import org.junit.Test;

public abstract class AbstractSwingUtilsTestIsMouseButton {
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
  protected final int[] mouseEventParams;
  /**
   * index 0: result for isMouseLeft
   * index 1: result for isMouseRight
   */
  protected final boolean[] expectedResult;

  public AbstractSwingUtilsTestIsMouseButton(int[] mouseEventParams,
                                             boolean[] expectedResult) {
    this.mouseEventParams = mouseEventParams;
    this.expectedResult = expectedResult;
  }

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

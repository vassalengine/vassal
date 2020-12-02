package VASSAL.configure;

import VASSAL.counters.TraitConfigPanel;

/**
 * A standardised Panel for use by Component configurers
 */
public class ComponentConfigPanel extends TraitConfigPanel {
  private static final long serialVersionUID = 1L;

  /**
   * Create a new default Component Config Panel
   */
  public ComponentConfigPanel() {
    this(false);
  }

  /**
   * Create a new default Trait Config Panel and specify debug option
   *
   * @param debug Turn debug on?
   */
  public ComponentConfigPanel(boolean debug) {
    super(new ComponentLayout(debug));
  }

  /**
   * Create a new Trait Config Panel with a non-standard layout
   *
   * @param layout Non-standard layout
   */
  public ComponentConfigPanel(ComponentLayout layout) {
    super(layout);
  }
}

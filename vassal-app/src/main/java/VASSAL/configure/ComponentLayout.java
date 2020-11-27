package VASSAL.configure;


import VASSAL.counters.TraitLayout;

/**
 * A standardised MigLayout for use by Component configurers
 */
public class ComponentLayout extends TraitLayout {
  private static final long serialVersionUID = 1L;

  public static final String COMPONENT_INSETS = "ins panel"; // NON-NLS
  public static final String DEFAULT_COMPONENT_LAYOUT_CONSTRAINTS = COMPONENT_INSETS + "," +  STANDARD_GAPY + ",hidemode 3,wrap 2"; // NON-NLS
  public static final String DEFAULT_COMPONENT_COLUMN_CONSTRAINTS = "[right]rel[fill,grow]"; // NON-NLS

  /**
   * Create a standardised 2 column Component layout that will suit most traits.
   */
  public ComponentLayout() {
    this(false);

  }

  /**
   * Create a standardised 2 column Component layout that will suit most traits.
   *
   * @param debug Turn layout debug option on?
   */
  public ComponentLayout(boolean debug) {
    super(debug, DEFAULT_COMPONENT_LAYOUT_CONSTRAINTS, DEFAULT_COMPONENT_COLUMN_CONSTRAINTS);
  }


}

package VASSAL.configure;

import static VASSAL.configure.BeanShellExpressionConfigurer.Option;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.map.BoardPicker;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.Properties;
import VASSAL.script.expression.FunctionBuilder;
import VASSAL.script.expression.IntBuilder;
import VASSAL.script.expression.StrBuilder;
import VASSAL.tools.menu.MenuScroller;
import VASSAL.tools.swing.SwingUtils;

import java.awt.Dimension;
import java.util.Arrays;
import java.util.List;

import javax.swing.JDialog;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class BeanShellFunctionMenu extends JPopupMenu {

  protected static final String[] SUM_COUNT_HINTS = new String[]{"WARNING - This function needs to scan many pieces and is relatively slow. Use sparingly", "$property$ variables may be used in the Match Expression and will be evaluated on the source piece.", "See the Reference Manual for detailed usage."};

  protected static int maxScrollItems = 0;
  protected BeanShellExpressionConfigurer configurer;
  protected EditablePiece target;

  public BeanShellFunctionMenu (EditablePiece target, BeanShellExpressionConfigurer configurer) {
    super();

    this.target = target;
    this.configurer = configurer;

    final JMenu constantMenu = new JMenu("Constant");
    final JMenuItem integerItem = new JMenuItem("Number");
    integerItem.setToolTipText("A number");
    integerItem.addActionListener(e -> buildInteger());
    constantMenu.add(integerItem);

    final JMenuItem stringItem = new JMenuItem("String");
    stringItem.setToolTipText("A character string");
    stringItem.addActionListener(e -> buildString());
    constantMenu.add(stringItem);
    add(constantMenu);


    final JMenu propertyMenu = new JMenu("Property");

    if (target != null) {
      final JMenu pieceMenu = new JMenu("Piece Property");
      addProp(pieceMenu, Properties.MOVED);
      addProp(pieceMenu, Properties.SELECTED);
      addProp(pieceMenu, Properties.PIECE_ID);
      addPieceProps(pieceMenu, target);
      propertyMenu.add(pieceMenu);
    }

    final JMenu globalsMenu = new JMenu("Global Property");
    buildGlobalMenu(globalsMenu, GameModule.getGameModule(), true);
    MenuScroller.setScrollerFor(globalsMenu, getMaxScrollItems(), 100);
    propertyMenu.add(globalsMenu);

    final JMenu vassalMenu = new JMenu("Vassal Property");
    addProp(vassalMenu, GlobalOptions.PLAYER_SIDE);
    addProp(vassalMenu, GlobalOptions.PLAYER_NAME);
    addProp(vassalMenu, GlobalOptions.PLAYER_ID);
    propertyMenu.add(vassalMenu);

    add(propertyMenu);

    final JMenu operatorMenu = new JMenu("Operator");
    addOperator(operatorMenu, "+", "Add");
    addOperator(operatorMenu, "-", "Subtract");
    addOperator(operatorMenu, "*", "Multiply");
    addOperator(operatorMenu, "/", "Divide");
    addOperator(operatorMenu, "%", "Modulus");
    add(operatorMenu);

    final JMenu comparisonMenu = new JMenu("Comparison");
    addOperator(comparisonMenu, "==", "Equals");
    addOperator(comparisonMenu, "!=", "Not equals");
    addOperator(comparisonMenu, ">",  "Greater than");
    addOperator(comparisonMenu, ">=", "Greater than or equal to");
    addOperator(comparisonMenu, "<",  "Less than");
    addOperator(comparisonMenu, "<=", "Less than or equal to");
    addOperator(comparisonMenu, "=~", "Matches Regular Expression");
    addOperator(comparisonMenu, "!~", "Does not match Regular Expression");
    add(comparisonMenu);

    final JMenu logicalMenu = new JMenu("Logical");
    addOperator(logicalMenu, "&&", "And");
    addOperator(logicalMenu, "||", "Or");
    addOperator(logicalMenu, "!", "Not");
    addOperator(logicalMenu, "(", "Left parenthesis");
    addOperator(logicalMenu, ")", "Right parenthesis");
    add(logicalMenu);

    final JMenu mathMenu = new JMenu("Math");
    addFunction(mathMenu, "Math.abs", "Absolute value of a number", new String[] { "Number"}, "(n)");
    addFunction(mathMenu, "Math.min", "Minimum of two numbers", new String[] { "Number 1", "Number 2" }, "(m, n)");
    addFunction(mathMenu, "Math.max", "Maximum of two numbers", new String[] { "Number 1", "Number 2" }, "(m, n)");

    final JMenu propMenu = new JMenu("Property");
    addFunction(propMenu, "GetProperty", "Get the value of a property", new String[] { "Property name" }, "(prop)");
    addFunction(propMenu, "GetMapProperty", "Get the value of a Map-level property", new String[] { "Property name", "Map name" }, "(prop, map)");
    addFunction(propMenu, "GetZoneProperty", "Get the value of a Zone-level property on the current Map", new String[] { "Property name", "Zone name" }, "(prop, zone)");
    addFunction(propMenu, "GetZoneProperty", "Get the value of a Zone-level property on any Map", new String[] { "Property name", "Zone name", "Map name" }, "(prop, zone, map)");

    final JMenu stringMenu = new JMenu("String");
    addFunction(stringMenu, ".length", "Return the length of a string", new String[] {"Target String"}, "()");
    addFunction(stringMenu, ".contains", "Return true if string contains the specified search string", new String[] { "Target String", "Search String" }, "(string)");
    addFunction(stringMenu, ".startsWith", "Return true if string starts with the specified search string", new String[] { "Target String", "Search String" }, "(string)");
    addFunction(stringMenu, ".endsWith", "Return true if string ends with the specified search string", new String[] { "Target String", "String String" }, "(string)");
    addFunction(stringMenu, ".matches", "Return true if string matches the specified Regular Expression", new String[] { "Target String", "Regular Expression" }, "(regExpr)");
    addFunction(stringMenu, ".indexOf", "Return the position of the specified search string", new String[] { "Target String", "Search String" }, "(string)");
    addFunction(stringMenu, ".lastIndexOf", "Return the last position of the specified search string", new String[] { "Target String", "Search String" }, "(string)");
    addFunction(stringMenu, ".substring", "Return substring starting at a given position, up to the end of the string", new String[] { "Target String", "Starting position" }, "(start)");
    addFunction(stringMenu, ".substring", "Return substring between positions", new String[] { "Target String", "Starting position", "End Position" }, "(start, end)");
    addFunction(stringMenu, ".replace", "Replace one substring with another", new String[] { "Target String", "String to find", "String to replace" }, "(old, new)");

    final JMenu randomMenu = new JMenu("Random");
    addFunction(randomMenu, "Random", "Return a random whole number between 1 and a number (inclusive)", new String[] { "Highest number" }, "(x)");
    addFunction(randomMenu, "Random", "Return a random whole number between 2 numbers (inclusive)", new String[] { "Lowest number", "Highest number" }, "(x, y)");
    addFunction(randomMenu, "IsRandom", "Return true randomly 50% of the time", new String[] {}, "()");
    addFunction(randomMenu, "IsRandom", "Return true randomly a specified % of the time", new String[] { "Percent" }, "(p)");

    final JMenu countMenu = new JMenu("Sum & Count");
    addFunction(countMenu, "SumStack", "Sum the values of the named property in all pieces in the same stack", new String[] { "Property name" }, "(name)");
    addFunction(countMenu, "Sum", "Sum the values of the named property in matching pieces on all maps", new String[] { "Property name", "Property match expression" }, "(name, expr)", SUM_COUNT_HINTS, new Option[] {Option.NONE, Option.PME});
    addFunction(countMenu, "Sum", "Sum the values of the named property in matching pieces on the named map", new String[] { "Property name", "Property match expression", "Map Name" }, "(name, expr, map)", SUM_COUNT_HINTS, new Option[] {Option.NONE, Option.PME, Option.NONE});
    addFunction(countMenu, "Count", "Count the number of matching pieces on all maps", new String[] { "Property match expression" }, "(expr)", SUM_COUNT_HINTS, new Option[] {Option.PME});
    addFunction(countMenu, "Count", "Count the number of matching pieces on the named map", new String[] { "Property match expression", "Map Name" }, "(expr, map)", SUM_COUNT_HINTS, new Option[] {Option.PME, Option.NONE});

    final JMenu functionMenu = new JMenu("Function");
    functionMenu.add(mathMenu);
    functionMenu.add(propMenu);
    functionMenu.add(randomMenu);
    functionMenu.add(stringMenu);
    functionMenu.add(countMenu);
    addFunction(functionMenu, "?", "Return a different result depending on a logical expression", new String[] { "Logical expression", "Result if true", "Result if false" }, "(expr ? r1 : r2)");
    addFunction(functionMenu, "Alert", "Display text in a Dialog box", new String[] { "Text to display" }, "(text)");

    add(functionMenu);
  }

  protected void addFunction(JMenu menu, final String op, final String desc, final String[] parms, final String parmInfo) {
    addFunction(menu, op, desc, parms, parmInfo, null);
  }

  protected void addFunction(JMenu menu, final String op, final String desc, final String[] parms, final String parmInfo, final String[] hints) {
    final Option[] options = new Option[parms.length];
    Arrays.fill(options, Option.NONE);
    addFunction (menu, op, desc, parms, parmInfo, hints, options);
  }

  protected void addFunction(JMenu menu, final String op, final String desc, final String[] parms, final String parmInfo, final String[] hints, final Option[] options) {
    final JMenuItem item = new JMenuItem(op + ((parmInfo != null && parmInfo.length() == 0) ? "" : " " + parmInfo));
    item.setToolTipText(desc);
    item.addActionListener(e -> buildFunction(op, desc, parms, hints, options));
    menu.add(item);
  }

  protected void buildFunction(String op, String desc, String[] parmDesc, String[] hints, Option[] options) {
    if (parmDesc.length == 0) {
      configurer.insertName(op + "()");
    }
    else {
      final StringConfigurer result = new StringConfigurer(null, "", "");
      new FunctionBuilder(result, (JDialog) configurer.getTopLevelAncestor(), op, desc, parmDesc, target, hints, options, configurer.getSelectedText()).setVisible(true);
      if (result.getValue() != null && result.getValueString().length() > 0) {
        configurer.insertName(result.getValueString());
      }
    }
  }

  protected void buildInteger() {
    final StringConfigurer result = new StringConfigurer(null, "", "");
    new IntBuilder(result, (JDialog) configurer.getTopLevelAncestor()).setVisible(true);
    if (result.getValue() != null && result.getValueString().length() > 0) {
      configurer.insertName(result.getValueString());
    }
  }

  protected void buildString() {
    final StringConfigurer result = new StringConfigurer(null, "", "");
    new StrBuilder(result, (JDialog) configurer.getTopLevelAncestor()).setVisible(true);
    if (result.getValue() != null && result.getValueString().length() > 0) {
      configurer.insertName(result.getValueString());
    }
  }

  protected void addOperator(JMenu menu, final String op, String desc) {
    final JMenuItem item = new JMenuItem(op);
    item.setToolTipText(desc);
    item.addActionListener(e -> configurer.insertName(op));
    menu.add(item);
  }


  /**
   * Add straight property name to a menu
   * @param menu parent menu
   * @param propName property name to add
   */
  protected void addProp(JMenu menu, final String propName) {
    addProp(menu, propName, false);
  }

  protected void addProp(JMenu menu, final String propName, boolean sort) {
    // Ignore any null propNames
    if (propName == null) {
      return;
    }

    final JMenuItem item = new JMenuItem(propName);
    item.addActionListener(e -> configurer.insertPropertyName(propName));
    if (sort) {
      int pos = -1;
      for (int i = 0; i < menu.getItemCount() && pos < 0; i++) {
        if (propName.compareTo(menu.getItem(i).getText()) <= 0) {
          pos = i;
        }
      }
      menu.add(item, pos);
    }
    else {
      menu.add(item);
    }
  }

  /**
   * Added the property names from an Editable Piece into their
   * own menu
   * @param menu parent menu
   * @param piece Piece containing property names
   */
  protected void addPieceProps(JMenu menu, EditablePiece piece) {
    if (piece == null) {
      return;
    }

    JMenu pieceMenu = null;

    if (piece instanceof PropertyNameSource) {
      List<String> propNames = ((PropertyNameSource) piece).getPropertyNames();
      for (String propName : propNames) {
        if (pieceMenu == null) {
          pieceMenu = new JMenu();
          pieceMenu.setText(piece.getDescription());
        }
        final JMenuItem item = new JMenuItem(propName);
        item.addActionListener(e -> configurer.insertPropertyName(((JMenuItem) e.getSource()).getText()));
        pieceMenu.add(item);
      }

      if (pieceMenu != null) {
        menu.add(pieceMenu);
      }

      if (piece instanceof Decorator) {
        addPieceProps(menu, (EditablePiece) ((Decorator) piece).getInner());
      }
    }
  }

  /**
   * Create a menu of Global Properties recorded in this module, based on
   * the module build structure
   */

  protected void buildGlobalMenu(JMenu parentMenu, AbstractBuildable target, boolean useParentMenu) {
    final List<Buildable> buildables = target.getBuildables();
    String menuName = ConfigureTree.getConfigureName(target.getClass());
    if (target instanceof AbstractConfigurable) {
      final String n = ((AbstractConfigurable) target).getConfigureName();
      if (n != null && n.length() > 0) {
        menuName += " " + n;
      }
    }
    final JMenu myMenu = new JMenu(menuName);

    final List<String> propNames = target.getPropertyNames();
    for (String propName : propNames) {
      addProp(useParentMenu ? parentMenu : myMenu, propName, true);
    }

    for (Buildable b : buildables) {
      if (b instanceof AbstractConfigurable) {
        // Remove 'filler' menu levels due to intermediate holding components
        final boolean useParent = (b instanceof GlobalProperties || b instanceof Board || b instanceof ZonedGrid);
        buildGlobalMenu(useParentMenu ? parentMenu : myMenu, (AbstractConfigurable) b, useParent);
      }
      else if (b instanceof BoardPicker) {
        buildGlobalMenu(myMenu, (AbstractBuildable) b, true);
      }
    }

    if (! useParentMenu & myMenu.getItemCount() > 0) {
      MenuScroller.setScrollerFor(myMenu, getMaxScrollItems(), 100);
      int pos = -1;
      for (int i = 0; i < parentMenu.getItemCount() && pos < 0; i++) {
        if (myMenu.getText().compareTo(parentMenu.getItem(i).getText()) <= 0) {
          pos = i;
        }
      }
      parentMenu.add(myMenu, pos);
    }
  }

  protected int getMaxScrollItems() {
    if (maxScrollItems == 0) {
      final Dimension itemSize = (new JMenuItem("Testing")).getPreferredSize();
      maxScrollItems = (int) (0.98 * SwingUtils.getScreenBounds(configurer.getControls()).height / itemSize.height);
    }
    return maxScrollItems;

  }
}

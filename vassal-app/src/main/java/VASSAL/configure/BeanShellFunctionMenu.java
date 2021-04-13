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
import VASSAL.i18n.Resources;
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
  private static final long serialVersionUID = 1L;

  protected static final String[] SUM_COUNT_HINTS = {
    Resources.getString("Editor.BeanShell.warning"),
    Resources.getString("Editor.BeanShell.warning2"),
    Resources.getString("Editor.BeanShell.warning3")
  };

  protected static final String[] SUM_COUNT_MAP_HINTS = {
    Resources.getString("Editor.BeanShell.warning"),
    Resources.getString("Editor.BeanShell.map_warning"),
    Resources.getString("Editor.BeanShell.warning2"),
    Resources.getString("Editor.BeanShell.warning3")
  };

  protected static int maxScrollItems = 0;
  protected BeanShellExpressionConfigurer configurer;
  protected EditablePiece target;

  public BeanShellFunctionMenu(EditablePiece target, BeanShellExpressionConfigurer configurer) {
    super();

    this.target = target;
    this.configurer = configurer;

    final JMenu constantMenu = new JMenu(Resources.getString("Editor.BeanShell.constant"));
    final JMenuItem integerItem = new JMenuItem(Resources.getString("Editor.BeanShell.number"));
    integerItem.setToolTipText(Resources.getString("Editor.BeanShell.a_number"));
    integerItem.addActionListener(e -> buildInteger());
    constantMenu.add(integerItem);

    final JMenuItem stringItem = new JMenuItem(Resources.getString("Editor.BeanShell.string"));
    stringItem.setToolTipText(Resources.getString("Editor.BeanShell.a_string"));
    stringItem.addActionListener(e -> buildString());
    constantMenu.add(stringItem);
    add(constantMenu);


    final JMenu propertyMenu = new JMenu(Resources.getString("Editor.BeanShell.property"));

    if (target != null) {
      final JMenu pieceMenu = new JMenu(Resources.getString("Editor.BeanShell.piece_property"));
      addProp(pieceMenu, Properties.MOVED);
      addProp(pieceMenu, Properties.SELECTED);
      addProp(pieceMenu, Properties.PIECE_ID);
      addPieceProps(pieceMenu, target);
      propertyMenu.add(pieceMenu);
    }

    final JMenu globalsMenu = new JMenu(Resources.getString("Editor.BeanShell.global_property"));
    buildGlobalMenu(globalsMenu, GameModule.getGameModule(), true);
    MenuScroller.setScrollerFor(globalsMenu, getMaxScrollItems(), 100);
    propertyMenu.add(globalsMenu);

    final JMenu vassalMenu = new JMenu(Resources.getString("Editor.BeanShell.vassal_property"));
    addProp(vassalMenu, GlobalOptions.PLAYER_SIDE);
    addProp(vassalMenu, GlobalOptions.PLAYER_NAME);
    addProp(vassalMenu, GlobalOptions.PLAYER_ID);
    propertyMenu.add(vassalMenu);

    add(propertyMenu);

    final JMenu operatorMenu = new JMenu(Resources.getString("Editor.BeanShell.operator"));
    addOperator(operatorMenu, "+", Resources.getString("Editor.BeanShell.add"));
    addOperator(operatorMenu, "-", Resources.getString("Editor.BeanShell.subtract"));
    addOperator(operatorMenu, "*", Resources.getString("Editor.BeanShell.multiply"));
    addOperator(operatorMenu, "/", Resources.getString("Editor.BeanShell.divide"));
    addOperator(operatorMenu, "%", Resources.getString("Editor.BeanShell.modulus"));
    add(operatorMenu);

    final JMenu comparisonMenu = new JMenu(Resources.getString("Editor.BeanShell.comparison"));
    addOperator(comparisonMenu, "==", Resources.getString("Editor.BeanShell.equals"));
    addOperator(comparisonMenu, "!=", Resources.getString("Editor.BeanShell.not_equals"));
    addOperator(comparisonMenu, ">",  Resources.getString("Editor.BeanShell.greater_than"));
    addOperator(comparisonMenu, ">=", Resources.getString("Editor.BeanShell.greater_than_or_equal_to"));
    addOperator(comparisonMenu, "<",  Resources.getString("Editor.BeanShell.less_than"));
    addOperator(comparisonMenu, "<=", Resources.getString("Editor.BeanShell.less_than_or_equal_to"));
    addOperator(comparisonMenu, "=~", Resources.getString("Editor.BeanShell.matches_regular_expression"));
    addOperator(comparisonMenu, "!~", Resources.getString("Editor.BeanShell.does_not_match_regular_expression"));
    add(comparisonMenu);

    final JMenu logicalMenu = new JMenu(Resources.getString("Editor.BeanShell.logical"));
    addOperator(logicalMenu, "&&", Resources.getString("Editor.BeanShell.and"));
    addOperator(logicalMenu, "||", Resources.getString("Editor.BeanShell.or"));
    addOperator(logicalMenu, "!", Resources.getString("Editor.BeanShell.not"));
    addOperator(logicalMenu, "(", Resources.getString("Editor.BeanShell.left_parenthesis"));
    addOperator(logicalMenu, ")", Resources.getString("Editor.BeanShell.right_parenthesis"));
    add(logicalMenu);

    final JMenu mathMenu = new JMenu(Resources.getString("Editor.BeanShell.math"));
    addFunction(mathMenu, "Math.abs", Resources.getString("Editor.BeanShell.abs"), new String[] { Resources.getString("Editor.BeanShell.number")}, "(n)"); //NON-NLS
    addFunction(mathMenu, "Math.min", Resources.getString("Editor.BeanShell.min"), new String[] { Resources.getString("Editor.BeanShell.number1"), Resources.getString("Editor.BeanShell.number2") }, "(m, n)"); //NON-NLS
    addFunction(mathMenu, "Math.max", Resources.getString("Editor.BeanShell.max"), new String[] { Resources.getString("Editor.BeanShell.number1"), Resources.getString("Editor.BeanShell.number2") }, "(m, n)"); //NON-NLS

    final JMenu propMenu = new JMenu(Resources.getString("Editor.BeanShell.property"));
    addFunction(propMenu, "GetProperty", Resources.getString("Editor.BeanShell.getproperty"), new String[] { Resources.getString("Editor.BeanShell.property_name") }, "(prop)"); //NON-NLS
    addFunction(propMenu, "GetMapProperty", Resources.getString("Editor.BeanShell.getmapproperty"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.map_name") }, "(prop, map)"); //NON-NLS
    addFunction(propMenu, "GetZoneProperty", Resources.getString("Editor.BeanShell.getzoneproperty"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.zone_name") }, "(prop, zone)"); //NON-NLS
    addFunction(propMenu, "GetZoneProperty", Resources.getString("Editor.BeanShell.getzonemapproperty"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.zone_name"), Resources.getString("Editor.BeanShell.map_name") }, "(prop, zone, map)"); //NON-NLS

    final JMenu stringMenu = new JMenu(Resources.getString("Editor.BeanShell.string"));
    addFunction(stringMenu, ".length", Resources.getString("Editor.BeanShell.Slength"), new String[] {Resources.getString("Editor.BeanShell.target_string")}, "()"); //NON-NLS
    addFunction(stringMenu, ".contains", Resources.getString("Editor.BeanShell.Scontains"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.search_string") }, "(string)"); //NON-NLS
    addFunction(stringMenu, ".startsWith", Resources.getString("Editor.BeanShell.Sstartswith"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.search_string") }, "(string)"); //NON-NLS
    addFunction(stringMenu, ".endsWith", Resources.getString("Editor.BeanShell.Sendswith"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.search_string") }, "(string)"); //NON-NLS
    addFunction(stringMenu, ".matches", Resources.getString("Editor.BeanShell.Smatches"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.regex") }, "(regExpr)"); //NON-NLS
    addFunction(stringMenu, ".indexOf", Resources.getString("Editor.BeanShell.Sindexof"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.search_string") }, "(string)"); //NON-NLS
    addFunction(stringMenu, ".lastIndexOf", Resources.getString("Editor.BeanShell.Slastindexof"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.search_string") }, "(string)"); //NON-NLS
    addFunction(stringMenu, ".substring", Resources.getString("Editor.BeanShell.Ssubstring"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.starting_position") }, "(start)"); //NON-NLS
    addFunction(stringMenu, ".substring", Resources.getString("Editor.BeanShell.Ssubstring2"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.starting_position"), Resources.getString("Editor.BeanShell.ending_position") }, "(start, end)"); //NON-NLS
    addFunction(stringMenu, ".replace", Resources.getString("Editor.BeanShell.Sreplace"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.to_find"), Resources.getString("Editor.BeanShell.to_replace") }, "(old, new)"); //NON-NLS

    final JMenu randomMenu = new JMenu(Resources.getString("Editor.BeanShell.random"));
    addFunction(randomMenu, "Random", Resources.getString("Editor.BeanShell.random1"), new String[] { Resources.getString("Editor.BeanShell.randomhi") }, "(x)"); //NON-NLS
    addFunction(randomMenu, "Random", Resources.getString("Editor.BeanShell.random2"), new String[] { Resources.getString("Editor.BeanShell.randomlo"), Resources.getString("Editor.BeanShell.randomhi") }, "(x, y)"); //NON-NLS
    addFunction(randomMenu, "IsRandom", Resources.getString("Editor.BeanShell.random3"), new String[] {}, "()"); //NON-NLS
    addFunction(randomMenu, "IsRandom", Resources.getString("Editor.BeanShell.random4"), new String[] { Resources.getString("Editor.BeanShell.randomp") }, "(p)"); //NON-NLS

    final JMenu countMenu = new JMenu(Resources.getString("Editor.BeanShell.sumcount"));
    addFunction(countMenu, "SumStack", Resources.getString("Editor.BeanShell.sum1"), new String[] { Resources.getString("Editor.BeanShell.property_name") }, "(name)"); //NON-NLS
    addFunction(countMenu, "CountStack", Resources.getString("Editor.BeanShell.sum6"), new String[] { }, "()"); //NON-NLS
    addFunction(countMenu, "CountStack", Resources.getString("Editor.BeanShell.sum7"), new String[] { Resources.getString("Editor.BeanShell.property_name") }, "(name)"); //NON-NLS
    addFunction(countMenu, "Sum", Resources.getString("Editor.BeanShell.sum2"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(name, expr)", SUM_COUNT_HINTS, new Option[] {Option.NONE, Option.PME}); //NON-NLS
    addFunction(countMenu, "Sum", Resources.getString("Editor.BeanShell.sum3"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression"), Resources.getString("Editor.BeanShell.map_name") }, "(name, expr, map)", SUM_COUNT_MAP_HINTS, new Option[] {Option.NONE, Option.PME, Option.NONE}); //NON-NLS
    addFunction(countMenu, "Count", Resources.getString("Editor.BeanShell.sum4"), new String[] { Resources.getString("Editor.BeanShell.property_match_expression") }, "(expr)", SUM_COUNT_HINTS, new Option[] {Option.PME}); //NON-NLS
    addFunction(countMenu, "Count", Resources.getString("Editor.BeanShell.sum5"), new String[] { Resources.getString("Editor.BeanShell.property_match_expression"), Resources.getString("Editor.BeanShell.map_name") }, "(expr, map)", SUM_COUNT_MAP_HINTS, new Option[] {Option.PME, Option.NONE}); //NON-NLS

    final JMenu functionMenu = new JMenu(Resources.getString("Editor.BeanShell.function"));
    functionMenu.add(mathMenu);
    functionMenu.add(propMenu);
    functionMenu.add(randomMenu);
    functionMenu.add(stringMenu);
    functionMenu.add(countMenu);
    addFunction(functionMenu, "?", Resources.getString("Editor.BeanShell.ternary"), new String[] { Resources.getString("Editor.BeanShell.logical_expression"), Resources.getString("Editor.BeanShell.if_true"), Resources.getString("Editor.BeanShell.if_false") }, "(expr ? r1 : r2)"); //NON-NLS
    addFunction(functionMenu, "Alert", Resources.getString("Editor.BeanShell.alert"), new String[] { Resources.getString("Editor.BeanShell.text_to_display") }, "(text)"); //NON-NLS

    add(functionMenu);
  }

  protected void addFunction(JMenu menu, final String op, final String desc, final String[] parms, final String parmInfo) {
    addFunction(menu, op, desc, parms, parmInfo, null);
  }

  protected void addFunction(JMenu menu, final String op, final String desc, final String[] parms, final String parmInfo, final String[] hints) {
    final Option[] options = new Option[parms.length];
    Arrays.fill(options, Option.NONE);
    addFunction(menu, op, desc, parms, parmInfo, hints, options);
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
      final List<String> propNames = ((PropertyNameSource) piece).getPropertyNames();
      for (final String propName : propNames) {
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
    for (final String propName : propNames) {
      addProp(useParentMenu ? parentMenu : myMenu, propName, true);
    }

    for (final Buildable b : buildables) {
      if (b instanceof AbstractConfigurable) {
        // Remove 'filler' menu levels due to intermediate holding components
        final boolean useParent = (b instanceof GlobalProperties || b instanceof Board || b instanceof ZonedGrid);
        buildGlobalMenu(useParentMenu ? parentMenu : myMenu, (AbstractConfigurable) b, useParent);
      }
      else if (b instanceof BoardPicker) {
        buildGlobalMenu(myMenu, (AbstractBuildable) b, true);
      }
    }

    if (!useParentMenu && myMenu.getItemCount() > 0) {
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

/*
 *
 * Copyright (c) 2007-2020 by Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.configure;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.util.Arrays;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;
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
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.script.expression.FunctionBuilder;
import VASSAL.script.expression.IntBuilder;
import VASSAL.script.expression.StrBuilder;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.icon.IconFamily;
import VASSAL.tools.menu.MenuScroller;
import bsh.BeanShellExpressionValidator;

/**
 * A Configurer for Java Expressions
 */
public class BeanShellExpressionConfigurer extends StringConfigurer {

  /**
   * enum describing any special processing that needs to be done for particular expression types
   *
   * NONE = No special handling
   *  PME = Property Match Expression handling.
   */
  public enum Option {
    NONE, PME
  }

  protected static int maxScrollItems = 0;

  protected static final String[] SUM_COUNT_HINTS = new String[]{"WARNING - This function needs to scan many pieces and is relatively slow. Use sparingly", "$property$ variables may be used in the Match Expression and will be evaluated on the source piece.", "See the Reference Manual for detailed usage."};
  protected static final String[] STRING_HINTS = new String[]{"This function can only be used on a String or Function return value.", "Use GetProperty(\"propertyName\").function() to apply this function to a Property value."};

  protected JPanel expressionPanel;
  protected JPanel detailPanel;
  protected Validator validator;
  protected JButton extraDetails;
  protected Icon up;
  protected Icon down;
  protected StringConfigurer errorMessage;
  protected JLabel variables;
  protected JLabel methods;
  protected EditablePiece target;
  protected Option option;

  public BeanShellExpressionConfigurer(String key, String name, String val, GamePiece piece) {
    this(key, name, val, piece, Option.NONE );
  }

  public BeanShellExpressionConfigurer(String key, String name, String val, GamePiece piece, Option option) {
    super(key, name, val);
    this.option = option;
    if (piece instanceof EditablePiece) {
      target = (EditablePiece) piece;
    }
    else {
      target = null;
    }
    strip();
    up = IconFactory.getIcon("go-up", IconFamily.XSMALL);
    down = IconFactory.getIcon("go-down", IconFamily.XSMALL);
    extraDetails = new JButton("Insert");
    extraDetails.addActionListener(e -> doPopup());
  }

  protected void strip() {
    final String s = getValueString().trim();
    if (s.startsWith("{") && s.endsWith("}")) {
      setValue(s.substring(1, s.length()-1));
    }
  }

  public Option getOption() {
    return option;
  }

  @Override
  public String getValueString() {
    return (String) value;
  }

  @Override
  public void setValue(String s) {
    if (!noUpdate && nameField != null) {
      nameField.setText(s);
    }
    setValue((Object) s);
  }

  @Override
  public java.awt.Component getControls() {
    if (p == null) {
      expressionPanel = new JPanel(new MigLayout("fillx,ins 0", "[][grow][][]"));
      expressionPanel.add(new JLabel(getName()));
      validator = new Validator();
      nameField = new JTextField(30);
      nameField.setText(getValueString());
      expressionPanel.add(nameField, "growx");
      nameField.addKeyListener(new KeyAdapter() {
        @Override
        public void keyReleased(KeyEvent evt) {
          noUpdate = true;
          setValue(nameField.getText());
          validator.validate();
          noUpdate = false;
        }
      });
      expressionPanel.add(validator);
      expressionPanel.add(extraDetails, "wrap");
      validator.validate();

      detailPanel = new JPanel();
      detailPanel.setLayout(new BoxLayout(detailPanel, BoxLayout.Y_AXIS));

      errorMessage = new StringConfigurer(null, "Error Message:  ", "");
      errorMessage.getControls().setEnabled(false);
      variables = new JLabel("Vassal Properties:  ");
      methods = new JLabel("Methods:  ");

      detailPanel.add(errorMessage.getControls());
      detailPanel.add(variables);
      detailPanel.add(methods);
      detailPanel.setVisible(false);

      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
      p.add(expressionPanel);
      p.add(detailPanel);
    }
    return p;
  }

  protected void doPopup() {
    final JPopupMenu popup = createPopup();
    popup.show(extraDetails, 0, 0);
  }

  /**
   * Toggle the display of additional details
   */
  protected void toggleDetails() {
    extraDetails.setIcon(detailPanel.isVisible() ? down : up);
    detailPanel.setVisible(! detailPanel.isVisible());
    repack();
  }

  protected void repack() {
    final Window w = SwingUtilities.getWindowAncestor(p);
    if (w != null) {
      w.pack();
    }
  }

  /**
   * Build a popup menu
   * @return Generated popup menu
   */
  protected JPopupMenu createPopup() {
    JPopupMenu popup = new JPopupMenu();

    final JMenu constantMenu = new JMenu("Constant");
    final JMenuItem integerItem = new JMenuItem("Number");
    integerItem.setToolTipText("A number");
    integerItem.addActionListener(e -> buildInteger());
    constantMenu.add(integerItem);

    final JMenuItem stringItem = new JMenuItem("String");
    stringItem.setToolTipText("A character string");
    stringItem.addActionListener(e -> buildString());
    constantMenu.add(stringItem);
    popup.add(constantMenu);


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
    propertyMenu.add(globalsMenu);

    final JMenu vassalMenu = new JMenu("Vassal Property");
    addProp(vassalMenu, GlobalOptions.PLAYER_SIDE);
    addProp(vassalMenu, GlobalOptions.PLAYER_NAME);
    addProp(vassalMenu, GlobalOptions.PLAYER_ID);
    propertyMenu.add(vassalMenu);

    popup.add(propertyMenu);

    final JMenu operatorMenu = new JMenu("Operator");
    addOperator(operatorMenu, "+", "Add");
    addOperator(operatorMenu, "-", "Subtract");
    addOperator(operatorMenu, "*", "Multiply");
    addOperator(operatorMenu, "/", "Divide");
    addOperator(operatorMenu, "%", "Modulus");
    popup.add(operatorMenu);

    final JMenu comparisonMenu = new JMenu("Comparison");
    addOperator(comparisonMenu, "==", "Equals");
    addOperator(comparisonMenu, "!=", "Not equals");
    addOperator(comparisonMenu, ">",  "Greater than");
    addOperator(comparisonMenu, ">=", "Greater than or equal to");
    addOperator(comparisonMenu, "<",  "Less than");
    addOperator(comparisonMenu, "<=", "Less than or equal to");
    addOperator(comparisonMenu, "=~", "Matches Regular Expression");
    addOperator(comparisonMenu, "!~", "Does not match Regular Expression");
    popup.add(comparisonMenu);

    final JMenu logicalMenu = new JMenu("Logical");
    addOperator(logicalMenu, "&&", "And");
    addOperator(logicalMenu, "||", "Or");
    addOperator(logicalMenu, "(", "Left parenthesis");
    addOperator(logicalMenu, ")", "Right parenthesis");
    popup.add(logicalMenu);

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
    addFunction(stringMenu, ".length", "Return the length of a string", new String[] {}, "()");
    addFunction(stringMenu, ".contains", "Return true if string contains the specified search string", new String[] { "Search String" }, "(string)", STRING_HINTS);
    addFunction(stringMenu, ".startsWith", "Return true if string starts with the specified search string", new String[] { "Search String" }, "(string)", STRING_HINTS);
    addFunction(stringMenu, ".endsWith", "Return true if string ends with the specified search string", new String[] { "String String" }, "(string)", STRING_HINTS);
    addFunction(stringMenu, ".matches", "Return true if string matches the specified Regular Expression", new String[] { "Regular Expression" }, "(regExpr)", STRING_HINTS);
    addFunction(stringMenu, ".indexOf", "Return the position of the specified search string", new String[] { "Search String" }, "(string)", STRING_HINTS);
    addFunction(stringMenu, ".lastIndexOf", "Return the last position of the specified search string", new String[] { "Search String" }, "(string)", STRING_HINTS);
    addFunction(stringMenu, ".substring", "Return substring starting at a given position, up to the end of the string", new String[] { "Starting position" }, "(start)", STRING_HINTS);
    addFunction(stringMenu, ".substring", "Return substring between positions", new String[] { "Starting position", "End Position" }, "(start, end)", STRING_HINTS);
    addFunction(stringMenu, ".replace", "Replace one substring with another", new String[] { "String to find", "String to replace" }, "(old, new)", STRING_HINTS);

    final JMenu randomMenu = new JMenu("Random");
    addFunction(randomMenu, "Random", "Return a random whole number between 1 and a number (inclusive)", new String[] { "Highest number" }, "(x)");
    addFunction(randomMenu, "Random", "Return a random whole number between 2 numbers (inclusive)", new String[] { "Lowest number", "Highest number" }, "(x, y)");
    addFunction(randomMenu, "IsRandom", "Return true randomly 50% of the time", new String[] {}, "()");
    addFunction(randomMenu, "IsRandom", "Return true randomly a specified % of the time", new String[] { "Percent" }, "(p)");

    final JMenu countMenu = new JMenu("Sum & Count");
    addFunction(countMenu, "SumStack", "Sum the values of the named property in all pieces in the same stack", new String[] { "Property name" }, "(name)");
    addFunction(countMenu, "Sum", "Sum the values of the named property in matching pieces on all maps", new String[] { "Property name", "Property match expression" }, "(name, expr)", SUM_COUNT_HINTS, new Option[] {Option.NONE, Option.PME});
    addFunction(countMenu, "Sum", "Sum the values of the named property in matching pieces on the named map", new String[] { "Property name", "Property match expression", "Map Name" }, "(name, expr, map)", SUM_COUNT_HINTS, new Option[] {Option.NONE, Option.PME, Option.NONE});
    addFunction(countMenu, "Count", "Count the number of matching pieces on all maps", new String[] { "Property match expression" }, "(name, expr)", SUM_COUNT_HINTS, new Option[] {Option.PME});
    addFunction(countMenu, "Count", "Count the number of matching pieces on the named map", new String[] { "Property match expression", "Map Name" }, "(name, expr, map)", SUM_COUNT_HINTS, new Option[] {Option.PME, Option.NONE});

    final JMenu functionMenu = new JMenu("Function");
    functionMenu.add(mathMenu);
    functionMenu.add(propMenu);
    functionMenu.add(randomMenu);
    functionMenu.add(stringMenu);
    functionMenu.add(countMenu);
    addFunction(functionMenu, "?", "Return a different result depending on a logical expression", new String[] { "Logical expression", "Result if true", "Result if false" }, "(expr ? r1 : r2)");
    addFunction(functionMenu, "Alert", "Display text in a Dialog box", new String[] { "Text to display" }, "(text)");

    popup.add(functionMenu);

    return popup;
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
      insertName(op + "()");
    }
    else {
      final StringConfigurer result = new StringConfigurer(null, "", "");
      new FunctionBuilder(result, (JDialog) p.getTopLevelAncestor(), op, desc, parmDesc, target, hints, options).setVisible(true);
      if (result.getValue() != null && result.getValueString().length() > 0) {
        insertName(result.getValueString());
      }
    }
  }

  protected void buildInteger() {
    final StringConfigurer result = new StringConfigurer(null, "", "");
    new IntBuilder(result, (JDialog) p.getTopLevelAncestor()).setVisible(true);
    if (result.getValue() != null && result.getValueString().length() > 0) {
      insertName(result.getValueString());
    }
  }

  protected void buildString() {
    final StringConfigurer result = new StringConfigurer(null, "", "");
    new StrBuilder(result, (JDialog) p.getTopLevelAncestor()).setVisible(true);
    if (result.getValue() != null && result.getValueString().length() > 0) {
      insertName(result.getValueString());
    }
  }

  protected void addOperator(JMenu menu, final String op, String desc) {
    final JMenuItem item = new JMenuItem(op);
    item.setToolTipText(desc);
    item.addActionListener(e -> insertName(op));
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
    item.addActionListener(e -> insertPropertyName(propName));
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
        item.addActionListener(e -> insertPropertyName(((JMenuItem) e.getSource()).getText()));
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
        final boolean useParent = (b instanceof GlobalProperties || b instanceof Board ||b instanceof ZonedGrid);
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
      maxScrollItems = (int) (0.8 * Toolkit.getDefaultToolkit().getScreenSize().height/itemSize.height);
    }
    return maxScrollItems;

  }

  /**
   * Insert a property name into the expression
   * @param name property name
   */
  protected void insertPropertyName(String name) {
    insertName (cleanName(name));
  }

  protected void insertName(String name) {
    String work = nameField.getText();
    int pos = nameField.getCaretPosition();

    // Cut out any selected text
    if (nameField.getSelectedText() != null) {
      int start = nameField.getSelectionStart();
      int end = nameField.getSelectionEnd();
      work = work.substring(0, start) + work.substring(end);
      if (pos >= start && pos <= end) {
        pos = start;
      }
    }

    String news = work.substring(0, pos) + name + work.substring(pos);
    nameField.setText(news);
    nameField.setCaretPosition(pos + name.length());

    // Update the text field and repaint it
    noUpdate = true;
    setValue(nameField.getText());
    validator.validate();
    noUpdate = false;
    nameField.repaint();

    // Send focus back to text field
    nameField.requestFocusInWindow();
  }

  /*
   * If the property name is not a valid java variable name, it
   * needs to be returned using the GetProperty() function.
   */
  protected String cleanName(String name) {
    boolean valid = true;
    for (int i = 0; i < name.length() && valid; i++) {
      final char c = name.charAt(i);
      if (i==0) {
        valid = Character.isJavaIdentifierStart(c);
      }
      else {
        valid = Character.isJavaIdentifierPart(c);
      }
    }
    return valid ? name : "GetProperty(\""+name+"\")";
  }

  protected void setDetails(String error, List<String> v, List<String> m) {
    errorMessage.setValue(error);
    String s = "Vassal Properties:  " + (v == null ? "" : v.toString());
    variables.setText(s);
    s = "Methods:  " + (m == null ? "" : m.toString());
    methods.setText(s);
  }

  protected void setDetails() {
    setDetails ("", null, null);
  }
  /*
   * Class to check and reflect the validity of the current expression.
   */
  class Validator extends JLabel {

    protected static final int INVALID = 0;
    protected static final int VALID = 1;
    protected static final int UNKNOWN = 2;

    protected Icon tick;
    protected Icon cross;
    protected ImageIcon none;
    protected int status = UNKNOWN;
    protected boolean validating = false;
    protected boolean dirty = false;
    protected ValidationThread validationThread = new ValidationThread();

    private static final long serialVersionUID = 1L;

    public Validator() {
      cross = IconFactory.getIcon("no", IconFamily.XSMALL);
      tick = IconFactory.getIcon("yes", IconFamily.XSMALL);

      BufferedImage image = new BufferedImage(cross.getIconWidth(), cross.getIconHeight(), BufferedImage.TYPE_INT_ARGB);
      none = new ImageIcon(image);

      setStatus(UNKNOWN);
    }

    public void setStatus(int status) {
      if (status == VALID) {
        setIcon(tick);
      }
      else if (status == INVALID){
        setIcon(cross);
      }
      else {
        setIcon(none);
      }
      this.status = status;
    }

    public int getStatus() {
      return status;
    }

    /*
     *  Run the validation in a separate thread. If the expression is updated
     *  while validating, then revalidate.
     */
    @Override
    public void validate() {
      if (validating) {
        dirty = true;
      }
      else {
        validating = true;
        validator.setStatus(UNKNOWN);
        SwingUtilities.invokeLater(validationThread);
      }
    }

    class ValidationThread implements Runnable {

      @Override
      public void run() {
        if (getValueString().length() == 0) {
          validator.setStatus(UNKNOWN);
          setDetails();
        }
        else {
          BeanShellExpressionValidator v = new BeanShellExpressionValidator(getValueString());
          if (v.isValid()) {
            validator.setStatus(VALID);
          }
          else {
            validator.setStatus(INVALID);
          }
          setDetails(v.getError(), v.getVariables(), v.getMethods());
        }
        validating = false;
        if (dirty) {
          dirty = false;
          validate();
        }
      }
    }
  }
}

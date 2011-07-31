/*
 * $Id$
 *
 * Copyright (c) 2007-2009 by Brent Easton
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
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.util.HashMap;
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

import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.script.expression.FunctionBuilder;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.icon.IconFamily;
import bsh.BeanShellExpressionValidator;

/**
 * A Configurer for Java Expressions
 */
public class BeanShellExpressionConfigurer extends StringConfigurer {

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

  public BeanShellExpressionConfigurer(String key, String name) {
    this(key, name, "");
  }

  public BeanShellExpressionConfigurer(String key, String name, String val) {
    this(key, name, val, null);
  }

  public BeanShellExpressionConfigurer(String key, String name, String val, GamePiece piece) {
    super(key, name, val);
    if (piece instanceof EditablePiece) {
      target = (EditablePiece) piece;
    }
    else {
      target = new BasicPiece();
    }
    strip();
    up = IconFactory.getIcon("go-up", IconFamily.XSMALL);
    down = IconFactory.getIcon("go-down", IconFamily.XSMALL);
    extraDetails = new JButton("Insert");
    extraDetails.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        doPopup();
      }});
  }

  protected void strip() {
    final String s = getValueString().trim();
    if (s.startsWith("{") && s.endsWith("}")) {
      setValue(s.substring(1, s.length()-1));
    }
  }

  public String getValueString() {
    return (String) value;
  }

  public void setValue(String s) {
    if (!noUpdate && nameField != null) {
      nameField.setText(s);
    }
    setValue((Object) s);
  }

  public java.awt.Component getControls() {
    if (p == null) {
      expressionPanel = new JPanel();
      expressionPanel.setLayout(new BoxLayout(expressionPanel, BoxLayout.X_AXIS));
      expressionPanel.add(new JLabel(getName()));
      validator = new Validator();
      nameField = new JTextField(30);
      nameField.setMaximumSize
        (new Dimension(nameField.getMaximumSize().width,
                                nameField.getPreferredSize().height));
      nameField.setText(getValueString());
      expressionPanel.add(nameField);
      nameField.addKeyListener(new KeyAdapter() {
        public void keyReleased(KeyEvent evt) {
          noUpdate = true;
          setValue(nameField.getText());
          validator.validate();
          noUpdate = false;
        }
      });
      expressionPanel.add(validator);
      expressionPanel.add(extraDetails);
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
   * @return
   */
  protected JPopupMenu createPopup() {
    JPopupMenu popup = new JPopupMenu();

    final JMenu propertyMenu = new JMenu("Property");

    final JMenu pieceMenu = new JMenu("Piece Property");
    addProp(pieceMenu, Properties.MOVED);
    addProp(pieceMenu, Properties.SELECTED);
    addProp(pieceMenu, Properties.PIECE_ID);
    addPieceProps(pieceMenu, target);
    propertyMenu.add(pieceMenu);

    final JMenu globalsMenu = new JMenu("Global Property");
    addGlobalProps(globalsMenu);
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
    addOperator(comparisonMenu, ">", "Greater than");
    addOperator(comparisonMenu, ">=", "Greater than or equal to");
    addOperator(comparisonMenu, "<", "Less than");
    addOperator(comparisonMenu, "<=", "Less than or equal to");
    popup.add(comparisonMenu);

    final JMenu logicalMenu = new JMenu("Logical");
    addOperator(logicalMenu, "&&", "And");
    addOperator(logicalMenu, "||", "Or");
    addOperator(logicalMenu, "(", "Left parenthesis");
    addOperator(logicalMenu, ")", "Right parenthesis");
    popup.add(logicalMenu);

    final JMenu functionMenu = new JMenu("Function");
    addFunction(functionMenu, "Alert", "Display text in a Dialog box", new String[] {"Text to display"});
    addFunction(functionMenu, "Compare", "Compare two Strings or other objects", new String[]{"Object 1", "Object 2"});
    addFunction(functionMenu, "GetProperty", "Get a property by name", new String[]{"Property name"});
    addFunction(functionMenu, "If", "Return a different result depending on a logical expression", new String[]{"Logical expression", "Result if true", "Result if false"});
    addFunction(functionMenu, "SumStack", "Sum the values of the named property in all counters in the same stack", new String[]{"Property name"});
    popup.add(functionMenu);

    return popup;
  }

  protected void addFunction(JMenu menu, final String op, final String desc, final String[] parms) {
    final JMenuItem item = new JMenuItem(op);
    item.setToolTipText(desc);
    item.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent e) {
        buildFunction(op, desc, parms);
      }});
    menu.add(item);
  }

  protected void buildFunction(String op, String desc, String[] parmDesc) {
    final StringConfigurer result = new StringConfigurer(null, "", "");
    new FunctionBuilder(result, (JDialog) p.getTopLevelAncestor(), op, desc, parmDesc, target).setVisible(true);
  }

  protected void addOperator(JMenu menu, final String op, String desc) {
    final JMenuItem item = new JMenuItem(op);
    item.setToolTipText(desc);
    item.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent e) {
        insertPropertyName(op);
      }});
    menu.add(item);
  }
  /**
   * Add straight property name to a menu
   * @param menu parent menu
   * @param propName property name to add
   */
  protected void addProp(JMenu menu, final String propName) {
    final JMenuItem item = new JMenuItem(propName);
    item.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent e) {
        insertPropertyName(propName);
      }});
    menu.add(item);
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
        item.addActionListener(new ActionListener(){
          public void actionPerformed(ActionEvent e) {
            insertPropertyName(((JMenuItem) e.getSource()).getText());
          }});
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
   * structure BasicModule -> Map -> Zone
   * @param menu
   */
  protected void addGlobalProps(JMenu menu) {
    final HashMap<String, JMenu> mapMenus = new HashMap<String, JMenu>();
    final HashMap<String, HashMap<String, JMenu>> zoneHash = new HashMap<String, HashMap<String, JMenu>>();

    for (MutableProperty.Impl prop : MutableProperty.Impl.getAllProperties()) {
      MutablePropertiesContainer parent = prop.getParent();
      if (parent instanceof Zone) {
        final Map m = ((Zone) parent).getMap();

        HashMap<String, JMenu> zh = zoneHash.get(m.getMapName());
        if (zh == null) {
          zh = new HashMap<String, JMenu>();
          zoneHash.put(m.getMapName(), zh);
        }

        final String name = ((Zone) parent).getName();
        JMenu zm = zh.get(name);
        if (zm == null) {
            zm = new JMenu(name);
            zh.put(name, zm);
        }
        addProp(zm, prop.getName());

      }
      else if (parent instanceof GlobalProperties) {
        parent  = ((GlobalProperties) parent).getParent();
        if (parent instanceof Map) {
          final Map m = (Map) parent;
          JMenu mm = mapMenus.get(m.getMapName());
          if (mm == null) {
            mm = new JMenu(m.getMapName());
            mapMenus.put(m.getMapName(), mm);
          }
          addProp(mm, prop.getName());
        }
        else {
          addProp(menu, prop.getName());
        }
      }
      else {
        addProp(menu, prop.getName());
      }
    }

    for (String m : mapMenus.keySet()) {
      final JMenu mapMenu = mapMenus.get(m);
      final HashMap<String, JMenu> zoneMap = zoneHash.get(m);
      for (String zone : zoneMap.keySet()) {
        mapMenu.add(zoneMap.get(zone));
      }
      menu.add(mapMenu);
    }
  }

  /**
   * Insert a property name into the expression
   * @param name property name
   */
  protected void insertPropertyName(String name) {
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

      public void run() {
        if (getValueString().length() == 0) {
          validator.setStatus(UNKNOWN);
          setDetails();
        }
        else {
          BeanShellExpressionValidator v = new BeanShellExpressionValidator(getValueString());
          if (v.isValid()) {
            validator.setStatus(VALID);
            setDetails(v.getError(), v.getVariables(), v.getMethods());
          }
          else {
            validator.setStatus(INVALID);
            setDetails(v.getError(), v.getVariables(), v.getMethods());
          }
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

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

import VASSAL.script.expression.FunctionBuilder;
import java.awt.Component;
import java.awt.Window;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.icon.IconFamily;
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
  protected String selectedText;
  protected boolean displayOnly;
  protected FunctionBuilder builder;

  public BeanShellExpressionConfigurer(String key, String name, String val, GamePiece piece) {
    this(key, name, val, piece, Option.NONE );
  }

  public BeanShellExpressionConfigurer(String key, String name, String val, GamePiece piece, Option option) {
    this (key, name, val, piece, option, false);
  }

  public BeanShellExpressionConfigurer(String key, String name, String val, GamePiece piece, Option option, FunctionBuilder builder) {
    this (key, name, val, piece, option, false);
    this.builder = builder;
  }

  public BeanShellExpressionConfigurer(String key, String name, String val, GamePiece piece, Option option, boolean displayOnly) {
    super(key, name, val);
    this.option = option;
    this.displayOnly = displayOnly;
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
    extraDetails.addActionListener(e -> {
      setSelectedText(nameField.getSelectedText());
      doPopup();
    });
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

  protected Component getTopLevelAncestor() {
    return p.getTopLevelAncestor();
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
          updateParentBuilder();
        }
      });
      expressionPanel.add(validator);
      expressionPanel.add(extraDetails, "wrap");

      nameField.setEditable(! isDisplayOnly());
      extraDetails.setVisible(! isDisplayOnly());

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

  public void validate() {
    validator.validate();
  }

  /**
   * If we are the child of a FunctionBuilder, notify it we have changed.
   */
  protected void updateParentBuilder() {
    if (builder != null) {
      builder.update();
    }
  }

  protected void doPopup() {
    final JPopupMenu popup = new BeanShellFunctionMenu(target, this);
    popup.show(extraDetails, 0, 0);
  }

  protected void repack() {
    final Window w = SwingUtilities.getWindowAncestor(p);
    if (w != null) {
      w.pack();
    }
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
    final int selectionStart = nameField.getSelectionStart();
    final int selectionEnd = nameField.getSelectionEnd();

    // Cut out any selected text
    if (selectionStart != selectionEnd) {
      work = work.substring(0, selectionStart) + work.substring(selectionEnd);
      if (pos >= selectionStart && pos <= selectionEnd) {
        pos = selectionStart;
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
    updateParentBuilder();

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

  public String getSelectedText() {
    return selectedText;
  }

  public void setSelectedText(String selectedText) {
    this.selectedText = selectedText;
  }

  public boolean isDisplayOnly() {
    return displayOnly;
  }

  public void setDisplayOnly(boolean displayOnly) {
    this.displayOnly = displayOnly;
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
      repaint();
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

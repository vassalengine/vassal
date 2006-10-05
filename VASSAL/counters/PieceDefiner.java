/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.counters;

import java.awt.Dialog;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.module.documentation.HelpWindowExtension;
import VASSAL.build.widget.PieceSlot;

/**
 * This is the GamePiece designer dialog.  It appears when you edit
 * the properties of a "Single Piece" in the Configuration window.
 */
public class PieceDefiner extends javax.swing.JPanel implements HelpWindowExtension {
  protected static DefaultListModel availableModel;
  protected DefaultListModel inUseModel;
  protected ListCellRenderer r;
  protected PieceSlot slot;
  private GamePiece piece;
  private HelpWindow helpWindow;

  /** Creates new form test */
  public PieceDefiner() {
    if (availableModel == null) {
      availableModel = new DefaultListModel();
      availableModel.addElement(new BasicPiece());
      availableModel.addElement(new Delete());
      availableModel.addElement(new Clone());
      availableModel.addElement(new Embellishment());
      availableModel.addElement(new UsePrototype());
      availableModel.addElement(new Labeler());
      availableModel.addElement(new ReportState());
      availableModel.addElement(new TriggerAction());
      availableModel.addElement(new GlobalHotKey());
      availableModel.addElement(new ActionButton());
      availableModel.addElement(new FreeRotator());
      availableModel.addElement(new Pivot());
      availableModel.addElement(new Hideable());
      availableModel.addElement(new Obscurable());
      availableModel.addElement(new SendToLocation());
      availableModel.addElement(new CounterGlobalKeyCommand());
      availableModel.addElement(new Translate());
      availableModel.addElement(new ReturnToDeck());
      availableModel.addElement(new Immobilized());
      availableModel.addElement(new PropertySheet());
      availableModel.addElement(new TableInfo());
      availableModel.addElement(new PlaceMarker());
      availableModel.addElement(new Replace());
      availableModel.addElement(new NonRectangular());
      availableModel.addElement(new PlaySound());
      availableModel.addElement(new MovementMarkable());
      availableModel.addElement(new Footprint());
      availableModel.addElement(new AreaOfEffect());
      availableModel.addElement(new SubMenu());
      availableModel.addElement(new RestrictCommands());
      availableModel.addElement(new Restricted());
      availableModel.addElement(new Marker());
      availableModel.addElement(new DynamicProperty());
    }

    inUseModel = new DefaultListModel();
    r = new Renderer();
    slot = new PieceSlot();
    initComponents();
    availableList.setSelectedIndex(0);
  }

  public void setPiece(GamePiece piece) {
    inUseModel.clear();
    while (piece instanceof Decorator) {
      inUseModel.insertElementAt(piece, 0);
      boolean contains = false;
      for (int i = 0,j = availableModel.size(); i < j; ++i) {
        if (piece.getClass().isInstance(availableModel.elementAt(i))) {
          contains = true;
          break;
        }
      }
      if (!contains) {
        try {
          availableModel.addElement(piece.getClass().newInstance());
        }
        catch (Throwable ex) {
        }
      }
      piece = ((Decorator) piece).piece;
    }
    if (piece == null) {
      inUseModel.insertElementAt(new BasicPiece(), 0);
    }
    else {
      inUseModel.insertElementAt(piece, 0);
    }
    inUseList.setSelectedIndex(0);
    refresh();
  }

  public void setBaseWindow(HelpWindow w) {
    helpWindow = w;
  }

  private void refresh() {
    if (inUseModel.getSize() > 0) {
      piece = (GamePiece) inUseModel.lastElement();
    }
    else {
      piece = null;
    }
    slot.setPiece(piece);
    slot.getComponent().repaint();
  }

  public GamePiece getPiece() {
    return piece;
  }

  /** This method is called from within the constructor to
   * initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is
   * always regenerated by the FormEditor.
   */
  private void initComponents() {
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    add(slot.getComponent());

    JPanel controls = new JPanel();
    controls.setLayout(new BoxLayout(controls, BoxLayout.X_AXIS));

    availablePanel = new javax.swing.JPanel();
    availableScroll = new javax.swing.JScrollPane();
    availableList = new javax.swing.JList();
    helpButton = new javax.swing.JButton();
    importButton = new javax.swing.JButton();
    addRemovePanel = new javax.swing.JPanel();
    addButton = new javax.swing.JButton();
    removeButton = new javax.swing.JButton();
    inUsePanel = new javax.swing.JPanel();
    inUseScroll = new javax.swing.JScrollPane();
    inUseList = new javax.swing.JList();
    propsButton = new javax.swing.JButton();
    moveUpDownPanel = new javax.swing.JPanel();
    moveUpButton = new javax.swing.JButton();
    moveDownButton = new javax.swing.JButton();
//        setLayout(new javax.swing.BoxLayout(this, 0));

    availablePanel.setLayout(new javax.swing.BoxLayout(availablePanel, 1));


    availableList.setModel(availableModel);
    availableList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    availableList.setCellRenderer(r);
    availableList.addListSelectionListener(new ListSelectionListener() {
      public void valueChanged(ListSelectionEvent evt) {
        Object o = availableList.getSelectedValue();
        helpButton.setEnabled(o instanceof EditablePiece
                              && ((EditablePiece) o).getHelpFile() != null);
        if (inUseModel.size() > 0) {
          addButton.setEnabled(o instanceof Decorator);
        }
        else {
          addButton.setEnabled(o != null);
        }
      }
    });

    availableScroll.setViewportView(availableList);
    availableScroll.setBorder(new TitledBorder("Available Traits"));

    availablePanel.add(availableScroll);


    helpButton.setText("Help");
    helpButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        showHelpForPiece();
      }
    }
    );
    availablePanel.add(helpButton);

    importButton.setText("Import");
    importButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        String className = JOptionPane.showInputDialog(PieceDefiner.this, "Enter fully-qualified name of Java class to import");
        importPiece(className);
      }
    }
    );

    availablePanel.add(importButton);

    controls.add(availablePanel);

    addRemovePanel.setLayout(new javax.swing.BoxLayout(addRemovePanel, 1));

    addButton.setText("Add ->");
    addButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        Object selected = availableList.getSelectedValue();
        if (selected instanceof Decorator) {
          if (inUseModel.getSize() > 0) {
            Decorator c = (Decorator) selected;
            addTrait(c);
            if (inUseModel.lastElement().getClass() == c.getClass()) {
              if (edit(inUseModel.size() - 1)) {   // Add was successful
              }
              else {                               // Add was cancelled
                if (!inUseModel.isEmpty())
                  removeTrait(inUseModel.size() - 1);
              }
            }
          }
        }
        else if (selected instanceof GamePiece
            && inUseModel.getSize() == 0) {
          try {
            GamePiece p = (GamePiece) selected.getClass().newInstance();
            setPiece(p);
            if (inUseModel.getSize() > 0) {
              if (edit(0)) {  // Add was successful
              }
              else {          // Add was cancelled
                removeTrait(0);
              }
            }
          }
          catch (Exception e) {
            e.printStackTrace();
          }
        }
      }
    }
    );
    addRemovePanel.add(addButton);


    removeButton.setText("<- Remove");
    removeButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        int index = inUseList.getSelectedIndex();
        if (index >= 0) {
          removeTrait(index);
          if (inUseModel.getSize() > 0) {
            inUseList.setSelectedIndex(Math.min(inUseModel.getSize() - 1, Math.max(index, 0)));
          }
        }
      }
    }
    );
    addRemovePanel.add(removeButton);


    controls.add(addRemovePanel);


    inUsePanel.setLayout(new javax.swing.BoxLayout(inUsePanel, 1));


    inUseList.setModel(inUseModel);
    inUseList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    inUseList.setCellRenderer(r);
    inUseList.addListSelectionListener(new ListSelectionListener() {
      public void valueChanged(ListSelectionEvent evt) {
        Object o = inUseList.getSelectedValue();
        int index = inUseList.getSelectedIndex();
        propsButton.setEnabled(o instanceof EditablePiece);
        if (inUseModel.size() > 1) {
          removeButton.setEnabled(index > 0);
        }
        else {
          removeButton.setEnabled(index == 0);
        }
        moveUpButton.setEnabled(index > 1);
        moveDownButton.setEnabled(index > 0
                                  && index < inUseModel.size() - 1);
      }
    });

    inUseList.addMouseListener(new MouseAdapter() {
      public void mouseReleased(MouseEvent evt) {
        if (evt.getClickCount() == 2) {
          int index = inUseList.locationToIndex(evt.getPoint());
          if (index >= 0) {
            edit(index);
          }
        }
      }
    });
    inUseScroll.setViewportView(inUseList);

    inUseScroll.setBorder(new TitledBorder("Current Traits"));

    inUsePanel.add(inUseScroll);


    propsButton.setText("Properties");
    propsButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        int index = inUseList.getSelectedIndex();
        if (index >= 0) {
          edit(index);
        }
      }
    }
    );
    inUsePanel.add(propsButton);


    controls.add(inUsePanel);


    moveUpDownPanel.setLayout(new javax.swing.BoxLayout(moveUpDownPanel, 1));

    moveUpButton.setText("Move Up");
    moveUpButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        int index = inUseList.getSelectedIndex();
        if (index > 1 && index < inUseModel.size()) {
          moveDecoratorUp(index);
        }
      }
    }
    );
    moveUpDownPanel.add(moveUpButton);


    moveDownButton.setText("Move Down");
    moveDownButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        int index = inUseList.getSelectedIndex();
        if (index > 0 && index < inUseModel.size() - 1) {
          moveDecoratorDown(index);
        }
      }
    }
    );
    moveUpDownPanel.add(moveDownButton);

    controls.add(moveUpDownPanel);

    add(controls);

  }

  protected void moveDecoratorDown(int index) {
    GamePiece selm1 = (GamePiece) inUseModel.elementAt(index - 1);
    Decorator sel = (Decorator) inUseModel.elementAt(index);
    Decorator selp1 = (Decorator) inUseModel.elementAt(index + 1);
    Decorator selp2 = index < inUseModel.size() - 2 ? (Decorator) inUseModel.elementAt(index + 2) : null;
    selp1.setInner(selm1);
    sel.setInner(selp1);
    if (selp2 != null) {
      selp2.setInner(sel);
    }
    inUseModel.setElementAt(selp1, index);
    inUseModel.setElementAt(sel, index + 1);
    inUseList.setSelectedIndex(index + 1);
    ((GamePiece) inUseModel.lastElement()).setProperty(Properties.OUTER, null);
    refresh();
  }

  protected void moveDecoratorUp(int index) {
    GamePiece selm2 = (GamePiece) inUseModel.elementAt(index - 2);
    Decorator sel = (Decorator) inUseModel.elementAt(index);
    Decorator selm1 = (Decorator) inUseModel.elementAt(index - 1);
    Decorator selp1 = index < inUseModel.size() - 1 ? (Decorator) inUseModel.elementAt(index + 1) : null;
    sel.setInner(selm2);
    selm1.setInner(sel);
    if (selp1 != null) {
      selp1.setInner(selm1);
    }
    inUseModel.setElementAt(selm1, index);
    inUseModel.setElementAt(sel, index - 1);
    ((GamePiece) inUseModel.lastElement()).setProperty(Properties.OUTER, null);
    inUseList.setSelectedIndex(index - 1);
    refresh();
  }

  protected void importPiece(String className) {
    try {
      Class c = GameModule.getGameModule().getDataArchive().loadClass(className);
      Object o = c.newInstance();
      if (o instanceof GamePiece) {
        availableModel.addElement(o);
      }
      else {
        JOptionPane.showMessageDialog(this, "Class must implement the GamePiece interface.", "Class error", JOptionPane.ERROR_MESSAGE);
      }
    }
    catch (ClassNotFoundException noClass) {
      JOptionPane.showMessageDialog(this, "Couldn't find class.\nClass file must exist in module zipfile with correct package structure.", "Class not found", JOptionPane.ERROR_MESSAGE);
    }
    catch (InstantiationException iex) {
      JOptionPane.showMessageDialog(this, "Couldn't instantiate class.\nClass must not be abstract.", "Class not initialized", JOptionPane.ERROR_MESSAGE);
    }
    catch (IllegalAccessException ilex) {
      JOptionPane.showMessageDialog(this, "Error accessing class", "Access error", JOptionPane.ERROR_MESSAGE);
    }
    catch (NoSuchMethodError noMethod) {
      JOptionPane.showMessageDialog(this, "Couldn't instantiate class.\nClass must have a no-argument constructor.", "Class not initialized", JOptionPane.ERROR_MESSAGE);
    }
  }

  private void showHelpForPiece() {
    Object o = availableList.getSelectedValue();
    if (o instanceof EditablePiece) {
      HelpFile h = ((EditablePiece) o).getHelpFile();
      if (helpWindow != null) {
        helpWindow.update(h.getContents());
        helpWindow.show();
      }
      else {
        h.showWindow();
      }
    }
  }

  protected boolean edit(int index) {
    Object o = inUseModel.elementAt(index);
    if (!(o instanceof EditablePiece)) {
      return false;
    }
    EditablePiece p = (EditablePiece) o;
    if (p.getEditor() != null) {
      Ed ed = null;
      Window w = SwingUtilities.getWindowAncestor(this);
      if (w instanceof Frame) {
        ed = new Ed((Frame) w, p);
      }
      else if (w instanceof Dialog) {
        ed = new Ed((Dialog) w, p);
      }
      else {
        ed = new Ed((Frame) null, p);
      }
      ed.setVisible(true);
      PieceEditor c = ed.getEditor();
      if (c != null) {
        p.mySetType(c.getType());
        if (p instanceof Decorator) {
          ((Decorator) p).mySetState(c.getState());
        }
        else {
          p.setState(c.getState());
        }
        refresh();
        return true;
      }
    }
    return false;
  }

  /** A Dialog for editing an EditablePiece's properties */
  protected static class Ed extends JDialog {
    PieceEditor ed;

    private Ed(Frame owner, final EditablePiece p) {
      super(owner, p.getDescription() + " properties", true);
      initialize(p);
    }

    private Ed(Dialog owner, final EditablePiece p) {
      super(owner, p.getDescription() + " properties", true);
      initialize(p);
    }

    private void initialize(final EditablePiece p) {
      ed = p.getEditor();
      getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      getContentPane().add(ed.getControls());
      JButton b = new JButton("Ok");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          dispose();
        }
      });
      JPanel panel = new JPanel();
      panel.add(b);
      b = new JButton("Cancel");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          ed = null;
          dispose();
        }
      });
      panel.add(b);
      getContentPane().add(panel);
      pack();
      setLocationRelativeTo(getOwner());
    }

    public PieceEditor getEditor() {
      return ed;
    }
  }

  protected void removeTrait(int index) {
    inUseModel.removeElementAt(index);
    if (index < inUseModel.size()) {
      ((Decorator) inUseModel.elementAt(index)).setInner((GamePiece) inUseModel.elementAt(index - 1));
    }
    refresh();
  }

  protected void addTrait(Decorator c) {
    try {
      c = (Decorator) c.getClass().newInstance();
      c.setInner((GamePiece) inUseModel.lastElement());
      inUseModel.addElement(c);
    }
    catch (Exception ex) {
      ex.printStackTrace();
    }
    refresh();
  }


  private javax.swing.JPanel availablePanel;
  private javax.swing.JScrollPane availableScroll;
  private javax.swing.JList availableList;
  private javax.swing.JButton helpButton;
  private javax.swing.JButton importButton;
  private javax.swing.JPanel addRemovePanel;
  private javax.swing.JButton addButton;
  private javax.swing.JButton removeButton;
  private javax.swing.JPanel inUsePanel;
  private javax.swing.JScrollPane inUseScroll;
  private javax.swing.JList inUseList;
  private javax.swing.JButton propsButton;
  private javax.swing.JPanel moveUpDownPanel;
  private javax.swing.JButton moveUpButton;
  private javax.swing.JButton moveDownButton;

  private static class Renderer extends DefaultListCellRenderer {
    public java.awt.Component getListCellRendererComponent
        (JList list, Object value, int index, boolean selected, boolean hasFocus) {
      super.getListCellRendererComponent(list, value, index, selected, hasFocus);
      if (value instanceof EditablePiece) {
        setText(((EditablePiece) value).getDescription());
      }
      else {
        String s = value.getClass().getName();
        setText(s.substring(s.lastIndexOf(".") + 1));
      }
      return this;
    }
  }
}

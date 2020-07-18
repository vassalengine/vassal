/*
 *
 * Copyright (c) 2005 by Rodney Kinney
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

package VASSAL.build.module.map.boardPicker.board;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.AffineTransform;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;

import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;
import VASSAL.build.module.map.boardPicker.board.mapgrid.RegularGridNumbering;
import VASSAL.i18n.Resources;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.swing.SwingUtils;

public abstract class GridEditor extends JDialog implements MouseListener, KeyListener {
  private static final long serialVersionUID = 1L;

  protected static final String SET = Resources.getString("Editor.GridEditor.set_grid_shape"); //$NON-NLS-1$
  protected static final String CANCEL = Resources.getString(Resources.CANCEL);
  protected static final String CANCEL_SET = Resources.getString("Editor.GridEditor.cancel_set"); //$NON-NLS-1$
  protected static final String OK = Resources.getString(Resources.SAVE);
  protected static final String NUMBERING = Resources.getString("Editor.GridEditor.numbering"); //$NON-NLS-1$

  protected EditableGrid grid;
  protected Board board;

  protected JPanel view;
  protected JScrollPane scroll;

  protected boolean setMode;
  protected Point hp1, hp2, hp3;

  protected JButton okButton, canSetButton, setButton, numberingButton;

  protected boolean saveGridVisible, saveNumberingVisible;
  protected double saveDx, saveDy;
  protected Point saveOrigin;

  public GridEditor(EditableGrid grid) {
    super();
    setTitle(Resources.getString("Editor.ModuleEditor.edit", grid.getGridName())); //$NON-NLS-1$
    setModal(true);
    this.grid = grid;
    GridContainer container = grid.getContainer();
    if (container != null) {
      board = container.getBoard();
    }
    saveGridVisible = grid.isVisible();
    if (grid.getGridNumbering() != null) {
      saveNumberingVisible = grid.getGridNumbering().isVisible();
      // if (saveGridVisible) {
      //  ((RegularGridNumbering) grid.getGridNumbering()).setAttribute(RegularGridNumbering.VISIBLE, Boolean.FALSE);
      // }
    }

    saveDx = grid.getDx();
    saveDy = grid.getDy();
    saveOrigin = grid.getOrigin();

    initComponents();
  }

  protected void initComponents() {
    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent we) {
        cancel();
      }
    });

    view = new GridPanel(board);

    view.addMouseListener(this);
    view.addKeyListener(this);
    view.setFocusable(true);

    scroll = new AdjustableSpeedScrollPane(
      view,
      JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
      JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

    scroll.setPreferredSize(new Dimension(800, 600));
    add(scroll, BorderLayout.CENTER);

    Box textPanel = Box.createVerticalBox();
    textPanel.add(new JLabel(Resources.getString("Editor.GridEditor.arrow_keys"))); //$NON-NLS-1$
    textPanel.add(new JLabel(Resources.getString("Editor.GridEditor.control_arrow_keys"))); //$NON-NLS-1$
    textPanel.add(new JLabel(Resources.getString("Editor.GridEditor.shift_key"))); //$NON-NLS-1$

    JPanel buttonPanel = new JPanel();

    okButton = new JButton(OK);
    okButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        cancelSetMode();
        setVisible(false);
/*
        GameModule.getGameModule()
                  .getDataArchive().clearTransformedImageCache();
*/
      }
    });
    buttonPanel.add(okButton);

    JButton canButton = new JButton(CANCEL);
    canButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        cancel();
      }
    });
    buttonPanel.add(canButton);

    setButton = new JButton(SET);
    setButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        startSetMode();
      }
    });
    setButton.setRequestFocusEnabled(false);
    buttonPanel.add(setButton);

    canSetButton = new JButton(CANCEL_SET);
    canSetButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        cancelSetMode();
      }
    });
    canSetButton.setVisible(false);
    canSetButton.setRequestFocusEnabled(false);
    buttonPanel.add(canSetButton);


    numberingButton = new JButton(NUMBERING);
    numberingButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        ((RegularGridNumbering) grid.getGridNumbering()).setAttribute(RegularGridNumbering.VISIBLE, !grid.getGridNumbering().isVisible());
        repaint();
      }
    });
    numberingButton.setEnabled(grid.getGridNumbering() != null);
    numberingButton.setVisible(true);
    numberingButton.setRequestFocusEnabled(false);
    buttonPanel.add(numberingButton);

    Box controlPanel = Box.createVerticalBox();
    controlPanel.add(textPanel);
    controlPanel.add(buttonPanel);

    add(controlPanel, BorderLayout.SOUTH);

    scroll.revalidate();
    pack();
    repaint();
  }

  protected void cancel() {
    cancelSetMode();
    grid.setDx(saveDx);
    grid.setDy(saveDy);
    grid.setOrigin(saveOrigin);
    setVisible(false);
  }

  protected void cancelSetMode() {
    canSetButton.setVisible(false);
    setButton.setVisible(true);
    view.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    setMode = false;
    grid.setVisible(saveGridVisible);
    if (grid.getGridNumbering() != null && saveNumberingVisible) {
      ((RegularGridNumbering) grid.getGridNumbering()).setAttribute(RegularGridNumbering.VISIBLE, saveNumberingVisible);
    }
    repaint();
  }

  protected void startSetMode() {
    hp1 = null;
    hp2 = null;
    hp3 = null;
    setMode = true;
    canSetButton.setVisible(true);
    setButton.setVisible(false);
    view.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
    grid.setVisible(false);
    JOptionPane.showMessageDialog(null,
        Resources.getString("Editor.GridEditor.click_on_3")); //$NON-NLS-1$
    repaint();
  }

  @Override
  public void keyPressed(KeyEvent e) {
    if (setMode) {
      return;
    }

    boolean sideways = grid.isSideways();

    switch (e.getKeyCode()) {
    case KeyEvent.VK_UP:
      if (e.isControlDown()) {
        if (sideways) {
          adjustDx(-1, e);
        }
        else {
          adjustDy(-1, e);
        }
      }
      else {
        if (sideways) {
          adjustX0(-1, e);
        }
        else {
          adjustY0(-1, e);
        }
      }
      break;
    case KeyEvent.VK_DOWN:
      if (e.isControlDown()) {
        if (sideways) {
          adjustDx(1, e);
        }
        else {
          adjustDy(1, e);
        }
      }
      else {
        if (sideways) {
          adjustX0(1, e);
        }
        else {
          adjustY0(1, e);
        }
      }
      break;
    case KeyEvent.VK_LEFT:
      if (e.isControlDown()) {
        if (sideways) {
          adjustDy(-1, e);
        }
        else {
          adjustDx(-1, e);
        }
      }
      else {
        if (sideways) {
          adjustY0(-1, e);
        }
        else {
          adjustX0(-1, e);
        }
      }
      break;
    case KeyEvent.VK_RIGHT:
      if (e.isControlDown()) {
        if (sideways) {
          adjustDy(1, e);
        }
        else {
          adjustDx(1, e);
        }
      }
      else {
        if (sideways) {
          adjustY0(1, e);
        }
        else {
          adjustX0(1, e);
        }
      }
      break;
    default :
      return;
    }


    repaint();
    e.consume();

  }

  public void rebuild() {

  }

  @Override
  public void keyReleased(KeyEvent e) {
    rebuild();
  }

  @Override
  public void keyTyped(KeyEvent e) {
  }

  @Override
  public void mouseClicked(MouseEvent e) {
    if (setMode && SwingUtils.isLeftMouseButton(e)) {
      if (hp1 == null) {
        hp1 = e.getPoint();
      }
      else if (hp2 == null) {
        hp2 = e.getPoint();
      }
      else if (hp3 == null) {
        hp3 = e.getPoint();
        calculate();
        cancelSetMode();
      }
      repaint();
    }
  }

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }

  @Override
  public void mousePressed(MouseEvent e) {
  }

  @Override
  public void mouseReleased(MouseEvent e) {

  }

  protected static final int DELTA = 1;
  protected static final double DDELTA = 0.1;
  protected static final int FAST = 5;
  protected static final int ERROR_MARGIN = 5;

  protected void adjustX0(int direction, KeyEvent e) {
    int delta = direction * DELTA;
    if (e.isShiftDown()) {
      delta *= FAST;
    }
    Point p = grid.getOrigin();
    setNewOrigin(new Point(p.x + delta, p.y));
  }

  protected void adjustY0(int direction, KeyEvent e) {
    int delta = direction * DELTA;
    if (e.isShiftDown()) {
      delta *= FAST;
    }
    Point p = grid.getOrigin();
    setNewOrigin(new Point(p.x, p.y + delta));
  }

  protected void adjustDx(int direction, KeyEvent e) {
    double delta = direction * DDELTA;
    if (e.isShiftDown()) {
      delta *= FAST;
    }
    grid.setDx(grid.getDx() + delta);
  }

  protected void adjustDy(int direction, KeyEvent e) {
    double delta = direction * DDELTA;
    if (e.isShiftDown()) {
      delta *= FAST;
    }
    grid.setDy(grid.getDy() + delta);
  }

  protected void setNewOrigin(Point p) {

    int width = (int) Math.round(grid.getDx());
    int height = (int) Math.round(grid.getDy());

    if (p.x < (-width)) {
      p.x += width;
    }
    else if (p.x > width) {
      p.x -= width;
    }

    if (p.y < (-height)) {
      p.y += height;
    }
    else if (p.y > height) {
      p.y -= height;
    }

    grid.setOrigin(p);
  }

  protected boolean isHorizontal(Point p1, Point p2) {
    return Math.abs(p2.y - p1.y) <= ERROR_MARGIN;
  }

  protected boolean isVertical(Point p1, Point p2) {
    return Math.abs(p2.x - p1.x) <= ERROR_MARGIN;
  }

  protected boolean isPerpendicular(Point p1, Point p2) {
    return isHorizontal(p1, p2) || isVertical(p1, p2);
  }

  protected void reportShapeError() {
    JOptionPane.showMessageDialog(null,
        Resources.getString("Editor.GridEditor.does_not_look", grid.getGridName()), //$NON-NLS-1$
        Resources.getString("Editor.GridEditor.grid_shape_error"), //$NON-NLS-1$
        JOptionPane.ERROR_MESSAGE);

  }

  /*
   * Calculate and set the Origin and size of the grid
   * based on the the 3 selected points.
   */
  public abstract void calculate();

  /*
   * Panel to display the Grid Editor
   */
  protected class GridPanel extends JPanel {

    private static final long serialVersionUID = 1L;
    protected Board board;

    public GridPanel() {
      super();
      setFocusTraversalKeysEnabled(false);
    }

    public GridPanel(Board b) {
      this();
      setBoard(b);
    }

    public void setBoard(Board b) {
      board = b;
      setSize(board.getSize());
      setPreferredSize(board.getSize());
    }

    public Board getBoard() {
      return board;
    }

    @Override
    public void paint(Graphics g) {
      if (board != null) {
        final Graphics2D g2d = (Graphics2D) g;
        final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
        final AffineTransform orig_t = g2d.getTransform();
        g2d.setTransform(SwingUtils.descaleTransform(orig_t));

        final Rectangle b = getVisibleRect();
        b.x *= os_scale;
        b.y *= os_scale;
        b.width *= os_scale;
        b.height *= os_scale;

        g.clearRect(b.x, b.y, b.width, b.height);
        board.draw(g, 0, 0, os_scale, this);
        g2d.setTransform(orig_t);

        if (setMode) {
          highlight(g, hp1);
          highlight(g, hp2);
          highlight(g, hp3);
        }
      }
      else {
        super.paint(g);
      }
    }

    protected void highlight(Graphics g, Point p) {
      final int r1 = 3;
      final int r2 = 10;

      if (p != null) {
        g.setColor(Color.red);
        g.fillOval(p.x-r1/2, p.y-r1/2, r1, r1);
        g.drawOval(p.x-r2/2, p.y-r2/2, r2, r2);
      }
    }

    @Override
    public boolean isFocusable() {
      return true;
    }
  }


  /*
   * Interface to be implemented by a class that wants to be edited
   * by RegularGridEditor
   */
  public interface EditableGrid {
    public double getDx();
    public double getDy();
    public Point getOrigin();

    public void setDx(double dx);
    public void setDy(double dy);
    public void setOrigin(Point p);

    public boolean isSideways();
    public void setSideways(boolean sideways);

    public GridContainer getContainer();
    public GridNumbering getGridNumbering();

    public boolean isVisible();
    public void setVisible(boolean b);

    public String getGridName();
  }



}

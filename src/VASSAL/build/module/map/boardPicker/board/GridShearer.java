/*
 * $Id$
 *
 * Copyright (c) 2011 by Pieter Geerkens 
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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.awt.geom.Point2D.Double;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;
import VASSAL.build.module.map.boardPicker.board.mapgrid.RegularGridNumbering;
import VASSAL.tools.AdjustableSpeedScrollPane;

@SuppressWarnings("serial")
public class GridShearer extends JDialog {
  //  private static final long serialVersionUID = 1L;
  private static final Logger logger =
        LoggerFactory.getLogger(GridShearer.class);
  protected static final String START = "Set Grid Shear";
  protected static final String CANCEL = "Cancel Grid Shear";
  protected static final String CLEAR = "Clear shear transform";
  protected static final String QUIT = "Quit";
  protected static final String SAVE = "Save";
  protected static final String NUMBERING = "Numbering";
  
  protected ShearableGrid grid;
  protected Board board;

  protected JPanel view;
  protected JScrollPane scroll;
  
  protected final Point fp1 = new Point(), 
                 fp2 = new Point(), 
                 sp = new Point(), 
                 spPrime = new Point();
  
  protected JButton 
    buttonShearStart, buttonShearCancel, buttonShearClear,
    buttonNumbering, buttonSave, buttonQuit;
  
  private enum Mode { 
    COMMAND("Ready"), 
    SEEK_FP1("Select the first fixed point."), 
    SEEK_FP2("Select the second fixed point."), 
    SEEK_SP("Select the point to be sheared."),
    DRAG("Click to fix the shear as displayed.");
    public final String message;
    private Mode(String s) { message = s; }
  }
  private Mode mode;
  
  private final StateMachine stateMachine = new  StateMachine();
  private final ActionExit actionExit = new ActionExit();
  private ShearTransformFactory shearTransformFactory;
  private AffineTransform shearTransform = AffineTransform.getQuadrantRotateInstance(0);
    
  protected boolean saveGridVisible, saveNumberingVisible;
  protected AffineTransform saveTransform;
  private JLabel labelStatus = new JLabel();
  private boolean wasCancelled;
  
  public GridShearer(ShearableGrid grid) {
    super();
    setTitle("Shear " + grid.getGridName());
    setModalityType(DEFAULT_MODALITY_TYPE);
    this.grid = grid;
    GridContainer container = grid.getContainer();
    if (container != null) { board = container.getBoard(); }
    
    // save grid status
    saveGridVisible = grid.isVisible();
    saveTransform = grid.getTransform();
    
    initComponents();
    view.addMouseListener(stateMachine);
    view.addMouseMotionListener(stateMachine);
    
    setMode(Mode.COMMAND);
  }
  
  public boolean cancelled() { return wasCancelled; }
  
  protected void initComponents() {
    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent we) { shutDown(true); }
    });

    view = new GridPanel(board);
    view.setFocusable(true);

    scroll = new AdjustableSpeedScrollPane(
        view,
        JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
        JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
    scroll.setPreferredSize(new Dimension(800, 600));
    add(scroll, BorderLayout.CENTER);

    Box textPanel = Box.createVerticalBox();
    textPanel.add(labelStatus);
    
    JPanel buttonPanel = new JPanel();
    
    buttonShearStart = new JButton(START);
    buttonShearStart.addActionListener(stateMachine);
    buttonShearStart.setRequestFocusEnabled(true);
    buttonShearStart.setToolTipText("Start defining a shear transformation");
    buttonPanel.add(buttonShearStart);

    buttonShearCancel = new JButton(CANCEL);
    buttonShearCancel.addActionListener(stateMachine);
    buttonShearCancel.setToolTipText("Cancel defining a shear transformation");
    buttonPanel.add(buttonShearCancel);

    buttonShearClear = new JButton(CLEAR);
    buttonShearClear.addActionListener(stateMachine);
    buttonShearClear.setToolTipText("Clear the current shear definition");
    buttonPanel.add(buttonShearClear);

    buttonNumbering = new JButton(NUMBERING);
    buttonNumbering.addActionListener(stateMachine);
    buttonNumbering.setEnabled(grid.getGridNumbering() != null);
    buttonNumbering.setToolTipText("Toggle display of numbering");
    buttonPanel.add(buttonNumbering);
    
    buttonSave = new JButton(SAVE);
    buttonSave.addActionListener(actionExit);
    buttonSave.setToolTipText("Exit and save this shear definition");
    buttonPanel.add(buttonSave);
    
    buttonQuit = new JButton(QUIT);
    buttonQuit.addActionListener(actionExit);
    buttonQuit.setToolTipText("Exit and discard this shear definition");
    buttonPanel.add(buttonQuit);
    
    Box controlPanel = Box.createVerticalBox();
    controlPanel.add(textPanel);
    controlPanel.add(buttonPanel);
    
    add(controlPanel, BorderLayout.SOUTH);

    scroll.revalidate();
    pack();
    repaint();
  }
 
   @Deprecated public void rebuild() {  }
  
// --------------------- State Machine ------------------------   
   private void setMode(Mode newMode) {
     this.mode = newMode;
     if (this.mode == Mode.COMMAND) { 
       shearTransformFactory = null; 
      resetPoints();
       buttonShearStart.setEnabled(true);
       buttonShearCancel.setEnabled(false);
       buttonShearClear.setEnabled(true);
     }
     else {
       buttonShearStart.setEnabled(false);
       buttonShearCancel.setEnabled(true);
       buttonShearClear.setEnabled(false);
     }
     labelStatus.setText("mode: " + mode.name() + " - " + mode.message);
     validate();
    repaint();
   }
   private void resetPoints() {
     fp1.setLocation(grid.getOrigin());
     fp2.setLocation(grid.getOrigin());
     sp.setLocation(grid.getOrigin());
     spPrime.setLocation(grid.getOrigin());
   }
   private class StateMachine extends MouseAdapter implements ActionListener {
     private final AffineTransform getNormal = AffineTransform.getQuadrantRotateInstance(1);
     private final Point delta = new Point();
     private final Point p1p2Normal = new Point();
     private boolean warningShown = false;
     private Line2D lineFp1Fp2;
     private int spOrientation;
     
     @Override
     public void mouseClicked(MouseEvent e) {   
       final Point p = e.getPoint();
       if (!view.contains(p)) return;
       switch(mode) {
         case SEEK_FP1: 
           fp1.setLocation(grid.snapTo(p));
           fp2.setLocation(fp1);
           setMode(Mode.SEEK_FP2);
           break;
         case SEEK_FP2:
           fp2.setLocation(grid.snapTo(p));
           if (fp2.distance(fp1) < grid.getDy())  break; // ignore out-of-sequence events
           delta.setLocation(fp2.x - fp1.x, fp2.y - fp1.y);
           getNormal.transform(delta,p1p2Normal);
           setMode(Mode.SEEK_SP);
           break;
         case SEEK_SP: 
           sp.setLocation(grid.snapTo(p));
           if (sp.distance(fp2) < grid.getDy())  break; // ignore out-of-sequence events
           try {
             shearTransformFactory = new ShearTransformFactory(fp1,fp2,sp);
           }
           catch (NoninvertibleTransformException e2) {
             JOptionPane.showMessageDialog(null,
               new StringBuilder("Cannot generate shear transform; try ")
               .append("again with points 1 & 2 in opposite corners of ")
               .append("the map and point 3 in one of the remaining corners."));
             setMode(Mode.COMMAND);
             break;
           }
           lineFp1Fp2 = new Line2D.Double(fp1,fp2);
           spOrientation = lineFp1Fp2.relativeCCW(sp);
           setMode(Mode.DRAG);
           break;
         case DRAG:
           setMode(Mode.COMMAND);
           break;
         default:  break; //throw new BrokenStateMachineException(mode,e.getClass());
       }
     }

     @Override public void mouseMoved(MouseEvent e) {
       final Point p = e.getPoint();
       if (!view.contains(p)) return;
       switch (mode) {
         case SEEK_FP1:  fp1.setLocation(grid.snapTo(p)); break; 
         case SEEK_FP2:  fp2.setLocation(grid.snapTo(p)); break; 
         case SEEK_SP:   sp.setLocation(grid.snapTo(p)); break;
         case DRAG:    if(lineFp1Fp2.relativeCCW(p) == spOrientation
                   && lineFp1Fp2.ptLineDist(p)  >= grid.getDy()) {
                    spPrime.setLocation(p);
                     shearTransform = shearTransformFactory.get(p);
                    try {
                      grid.setTransform(shearTransform);
                      warningShown = false;
                    }
                    catch (NoninvertibleTransformException e1) {
                       if (!warningShown) {
                         JOptionPane.showMessageDialog(null, 
                           "Don't go there!\nStay on your side of the red line.\n"); 
                       }
                       warningShown = true;
                    }
                   } else if (!warningShown) {
                     JOptionPane.showMessageDialog(null, 
                       "Don't go there!\nStay further away from the red line.\n"); 
                     warningShown = true;
                   }
                   break;
         default:     return;//throw new BrokenStateMachineException(mode,e.getClass());
       }
       repaint();
     }
     /** Handles the <b>StartShear</b>, <b>CancelShear</b>, and <b>Numbering</b> buttons */
    @Override
    public void actionPerformed(ActionEvent e) {
      if (e.getSource() == buttonShearStart) {
        setMode(Mode.SEEK_FP1);
      }
      else if (e.getSource() == buttonShearCancel) {
        setMode(Mode.COMMAND);
        shearTransform.setToIdentity();
      }
      else if (e.getSource() == buttonShearClear) {
        setMode(Mode.COMMAND);
        grid.clearTransform();
        shearTransform.setToIdentity();
      }
      else { // (e.getSource() == buttonNumbering) 
        ((RegularGridNumbering) grid.getGridNumbering()).setAttribute(
            RegularGridNumbering.VISIBLE, Boolean.valueOf(!grid.getGridNumbering().isVisible()));
      }
      repaint();
    }
   }
   /** Handles the <b>Save</b> and <b>Quit</b> buttons */
   private class ActionExit implements ActionListener {
     @Override
     public void actionPerformed(ActionEvent e) {
       shutDown(e.getSource() == buttonQuit);
     }
   }
   
   private void shutDown (boolean cancel) {
     // restore grid ...
     if (cancel) { 
       try {
         grid.setTransform(saveTransform);
       }
       catch (NoninvertibleTransformException e) {
         logger.error("NoninvertibleTransformException on restoring a saved " +
             "(valid) shear transformation\n",e); 
       }
     }
     wasCancelled = cancel;
     grid.setVisible(saveGridVisible);

     // ... then close down
     setVisible(false);
     view.getParent().remove(view);
   }

// --------------------- Shear Calculator ------------------------  
   /** 
    * Calculates an affine transform that holds two specified points <i>fp1</i> and <i>fp2</i>
    * fixed, while shearing/scaling a third point <i>sp</i> to <i>sp'</i>. The 
    * mathematics is simplified by constructing the net transform <i>Tx</i> as the 
    * concatenation <i>Ti * Ri * S * R * T</i> where:
    * <p><b>T</b> is the translation mapping <i>fp1</i> to the origin <i>O</i>;
    * <p><b>R</b> is the rotation mapping <i>T * fp2</i> onto the x-axis through <i>T * fp1</i>;
    * <p><b>S</b> is the shearing/scaling in the coordinate system from <i>R*T*sp</i> to <i>R*T*sp'</i>; and
    * <p><b>Ti</b> and <b>Ri</b> are the inverses respectively of <b>T</b> and <b>R</b>.
    * 
    * The following identities are readily seen to hold from the definitions
    * above, and can be used to quickly calculate matrix coefficients:
    * <p><b>T*fp1 == O</b>
    * <p><b>R*T*fp1 == O</b>
    * <p><b>R*T*fp2 == (r,0)</b> where r = |fp2-fp1|
    * <p><b>S*O == O</b>
    * <p><b>S*R*T*fp2 == R*T*fp2</b>
    * <p><b>S*R*T*sp == R*T*sp'</b>
    * @author Pieter Geerkens
    *
    */
  private class ShearTransformFactory{
    // for debugging
    private final Point origin = new Point(0,0);  // for assertions
    final Point2D R_T_fp2;
    
    private final double distance;
    @SuppressWarnings("hiding") private final Point fp1;
    @SuppressWarnings("hiding") private final Point fp2;
    @SuppressWarnings("hiding") private final Point sp;
    private final Point R_T_sp;
    private final AffineTransform R_T;
    private final AffineTransform Ti_Ri;
    /**
     * 
     * @param fp1 1st fixed point, about which the inner transform is calculated
     * @param fp2 2nd fixed point, rotated CW onto x-axis at (r,0) from p1 
     * @param sp 3rd (shear) point, which will be dragged to establish the shear
     * @throws NoninvertibleTransformException 
     */
    public ShearTransformFactory(Point fp1, Point fp2, Point sp) throws NoninvertibleTransformException{
      this.fp1 = fp1;
      this.fp2 = fp2;
      this.sp = sp;
      final AffineTransform translation = AffineTransform.getTranslateInstance(-fp1.x, -fp1.y);  
      
      distance = Double.distance(fp2.x, fp2.y, fp1.x, fp1.y);
      double cosTheta = (fp2.x - fp1.x)/distance, sinTheta = (fp2.y - fp1.y)/distance;
      R_T = new AffineTransform(cosTheta,-sinTheta, sinTheta,cosTheta, 0D,0D);
      R_T.concatenate(translation);
      
      Ti_Ri = R_T.createInverse();
      R_T_sp = (Point) R_T.transform(sp, new Point());

      {  // debugging
        R_T_fp2 = new Point2D.Double(distance,0);
        assert origin.equals(translation.transform(fp1, null)) : "O != T(fp1)";
        assert origin.equals(R_T.transform(fp1, null)) : "O != R(T(fp1))";
        assert R_T_fp2.distance(R_T.transform(fp2, null)) <= 1D : "r != R(T(fp2))";
      }
    }
    
    public AffineTransform get(@SuppressWarnings("hiding") Point spPrime) {
      final Point R_T_spPrime = (Point) R_T.transform(spPrime, new Point());
      
      final AffineTransform S = 
          new AffineTransform(1,0, (R_T_spPrime.getX()-R_T_sp.getX())/R_T_sp.getY(), R_T_spPrime.getY()/R_T_sp.getY(), 0,0);
      AffineTransform Ti_Ri_S_R_T = (AffineTransform) S.clone();
      Ti_Ri_S_R_T.concatenate(R_T);
      Ti_Ri_S_R_T.preConcatenate(Ti_Ri);
      
      { // debugging
        assert fp1.distance(Ti_Ri_S_R_T.transform(fp1, null)) < 1D : "fp1 != Ti(Ri(S(R(T(fp1)))))";
        assert fp2.distance(Ti_Ri_S_R_T.transform(fp2, null)) < 1D : "fp2 != Ti(Ri(S(R(T(fp2)))))";
        assert R_T_spPrime.distance(S.transform(R_T_sp,null)) < 2D : 
          String.format("R(T(spPrime)) != (S(R(T(sp))), distance: %1$8.2f\n", 
              R_T_spPrime.distance(S.transform(R_T_sp,null)));
        assert spPrime.distance(Ti_Ri_S_R_T.transform(sp, null)) < 2D : 
          String.format("sp' != Ti(Ri(S(R(T(sp))))), distance: %1$8.2f\n", 
              spPrime.distance(Ti_Ri_S_R_T.transform(sp, null)));
      }
      return Ti_Ri_S_R_T;
    }
  }
    
  protected void reportShearError() {
    JOptionPane.showMessageDialog(null,
        "Doesn't look like a " + grid.getGridName() + "!",
        "Grid Shear Error",
        JOptionPane.ERROR_MESSAGE);
  }
     
  /**
   * Panel to display the Grid Editor
   */
  protected class GridPanel extends JPanel {
    private static final long serialVersionUID = 1L;
    protected Board brd;

    public GridPanel() { setFocusTraversalKeysEnabled(false); }
    
    public GridPanel(Board b) { this();  setBoard(b); }

    public void setBoard(Board b) {
      brd = b;
      setSize(brd.getSize());
      setPreferredSize(brd.getSize());
    }

    public Board getBoard() { return brd; }

    @Override
    @SuppressWarnings("fallthrough")
    public void paint(Graphics g) {
      if (brd == null) {
        super.paint(g);
      }
      else {
        Rectangle b = getVisibleRect();
        g.clearRect(b.x, b.y, b.width, b.height);
        brd.draw(g, 0, 0, 1.0, this);

        Graphics2D g2d = (Graphics2D)g;
        @SuppressWarnings("hiding")
        AffineTransform saveTransform = g2d.getTransform();
        switch (mode) {
        case DRAG:
          if ( shearTransform != null) {
            AffineTransform transform = (AffineTransform) saveTransform.clone();
            transform.concatenate(shearTransform);
            g2d.setTransform(transform);
          }
          //$FALL-THROUGH$
        case SEEK_SP: 
          g.setColor(Color.blue);
          highlight(g, sp);
          g.drawLine(sp.x, sp.y, fp1.x, fp1.y);                       
          g.drawLine(sp.x, sp.y, fp2.x, fp2.y);
          //$FALL-THROUGH$
        case SEEK_FP2:
          g.setColor(Color.red);
          g.drawLine(fp1.x, fp1.y, fp2.x, fp2.y);                       
          highlight(g, fp2);
          //$FALL-THROUGH$
        case SEEK_FP1:
          g.setColor(Color.red);
          highlight(g, fp1);
          //$FALL-THROUGH$
        default:
          break;
        }
        g2d.setTransform(saveTransform);
      }
    }

    protected void highlight(Graphics g, Point p) {
      final int r1 = 3;
      final int r2 = 10;
      
      if (p != null) {
        g.fillOval(p.x-r1/2, p.y-r1/2, r1, r1);
        g.drawOval(p.x-r2/2, p.y-r2/2, r2, r2);
      }
    }
    
    @Override
    public boolean isFocusable() { return true;  }
  }

  /**
   * Interface to be implemented by a class that wants to be sheared
   * by <code>GridShearer</code>
   */
  public interface ShearableGrid extends IndexedHexGrid {
    String getGridName();
    
    boolean isVisible();
    void setVisible(boolean visible);
    GridNumbering getGridNumbering();
    
    AffineTransform getTransform();
    void setTransform(AffineTransform transform) throws NoninvertibleTransformException;
    void clearTransform();
    AffineTransform getInverse();
  }
}

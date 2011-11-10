/*
 * $Id$
 * 
 * Copyright (c) 2010-2011 by Pieter Geerkens
 * adapted from work Copyright (c) 2000-2005 by Rodney Kinney, Brent Easton
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.*;

import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.HexGridX;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.build.module.map.boardPicker.board.mapgrid.RegularGridNumbering.CoordType;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerWindow;
 
public class HexGridNumberingTest {

  /**
   * @param args  
   */
  public static void main(String[] args) {
    /** margin around grid */
    final int m = 15; 
    final int brdDefaultWidth = 1594;
    final int brdDefaultHeight = 840;

    class TestPanel extends JPanel {
      private static final long serialVersionUID = 1L; /* default */
      private boolean reversed;
      final private double cHexSize = 90.0;
      final private double cHexWidth = cHexSize * Math.sqrt(3.0)/2.0;
      final private Board brd;
      
      private HexGridX grid;
      private RegularGridNumbering numbering;;
      private HexGridNumberingX numberingX;
      private HexGridNumberingIrregular numberingIrregular;
      private Point pSnapTo;
      private Point pRangeStart = new Point(0,0);

      final JLabel lblBrdWidth = new JLabel("Board Width & Height:");
      final JTextField txtBrdWidth = new JTextField(""+brdDefaultWidth);
      final JTextField txtBrdHeight = new JTextField(""+brdDefaultHeight);

      final JTextField lblGridStatus = new JTextField("");

      final JTextField lblNumberingStatus = new JTextField("");
      
      final JTextField txtLocation = new JTextField("");
      
/*      public void setGridFields() {
        lblGridStatus.setText(String.format(
            " Height: %8$6.2f , Width: %7$6.2f " +
            "/ Wobbles=%1$3d, Straights=%2$3d, Obliques=%3$3d " +
            "/ Sideways: %4$s / Corners: %5$s / Edges: %6$s", 
            grid.maxWobble(), grid.maxStraight(), grid.maxOblique(),
            (grid.isSideways() ? " ON" : "OFF"),
            (grid.isCornersLegal() ? " ON" : "OFF"),
            (grid.isEdgesLegal() ? " ON" : "OFF"),
             grid.getHexSize(), grid.getHexWidth()) );
        lblGridStatus.setEnabled(false);
        lblGridStatus.setDisabledTextColor(getForeground());
        validate();
      }
*/
/*      public void setNumberingFields(){
        if (numbering.isOblique) {
          lblNumberingStatus.setText(String.format(
              " Oblique at: %1$3s / H-Descending: %2$3s / V-Descending: %3$3s",
              numbering.is_NW_SE ? "NW-SE" : "NE-SW",
              numbering.hDescending ? "ON" : "OFF",
              numbering.vDescending ? "ON" : "OFF"  ));
        } else {
          lblNumberingStatus.setText(String.format(
              " Rectangular w/ Stagger: %1$3s / H-Descending: %2$3s / V-Descending: %3$3s",
              numbering.isStagger ? "ON" : "OFF",
              numbering.hDescending ? "ON" : "OFF",
              numbering.vDescending ? "ON" : "OFF"  ));
        }
        lblNumberingStatus.setEnabled(false);
        lblNumberingStatus.setDisabledTextColor(getForeground());
      }
*/
      private String setLbl2Text(Point pCenter, Point pMouse) {
        return String.format(
          "  Range from %1$s: %2$2d  /   SnapTo = (%3$3d,%4$3d)  ", 
          grid.locationName(pCenter),
          grid.range(pCenter,pMouse),
          pSnapTo.x, pSnapTo.y
        );
      }
    
      private void initGrid(HexGridX g) {
        g.setAttribute(HexGrid.COLOR, Color.black);
        g.setDy(cHexSize);
        g.setDx(cHexWidth);
        g.setOrigin(new Point(10,30));
        g.setVisible(true);
      }
      private void initNumbering(RegularGridNumbering n) {
        n.setAttribute(RegularGridNumbering.COLOR, Color.black);
        n.setAttribute(RegularGridNumbering.FONT_SIZE, 10);
        n.setAttribute(RegularGridNumbering.H_TYPE,CoordType.ALPHABETIC);
        n.setAttribute(RegularGridNumbering.H_OFF,0);
        n.setAttribute(RegularGridNumbering.SEP, "-");
        n.setAttribute(RegularGridNumbering.VISIBLE, true);
      }
      
      private TestPanel() throws NumberFormatException, IllegalArgumentException, SecurityException {
        setLayout(new BorderLayout());
        Box box = Box.createHorizontalBox();
        Box b2 = Box.createHorizontalBox();

        txtBrdWidth.addKeyListener(new KeyAdapter() {
          @Override
          public void keyReleased(KeyEvent e) {
            try {
              brd.setAttribute(Board.WIDTH, Integer.parseInt(txtBrdWidth.getText()));
//              setGridFields();
              repaint();
            }
            catch (NumberFormatException e1) {
              e1.printStackTrace();
            }
          }
        });
        txtBrdWidth.setSize(50,txtBrdWidth.getHeight());

        txtBrdHeight.addKeyListener(new KeyAdapter() {
          @Override
          public void keyReleased(KeyEvent e) {
            try {
              brd.setAttribute(Board.HEIGHT, Integer.parseInt(txtBrdHeight.getText()));
//              setGridFields();
              repaint();
            }
            catch (NumberFormatException e1) {
              e1.printStackTrace();
            }
          }
        });
        txtBrdHeight.setSize(50,txtBrdHeight.getHeight());

        final JButton btnGridConfigure = new JButton("Grid");
        btnGridConfigure.setMnemonic(KeyEvent.VK_C);
        btnGridConfigure.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent e) {
            Configurer config = grid.getConfigurer();
            ConfigurerWindow window = new ConfigurerWindow(config,false);
            window.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
            window.addWindowListener(new WindowAdapter() {
              @Override
              public void windowClosed(WindowEvent evt) {
//                setGridFields();
              }
            });
            window.setModalityType(Dialog.ModalityType.APPLICATION_MODAL);
            window.setVisible(true);
          }
        });

        final JCheckBox chkIrregular = new JCheckBox("Irregular");
        chkIrregular.addItemListener(new ItemListener() {
          @Override
          public void itemStateChanged(ItemEvent e) {
            reversed = chkIrregular.isSelected();
            repaint();
          }
        });
        
        final JButton btnNumberingConfigure = new JButton("Numbering");
        btnNumberingConfigure.setMnemonic(KeyEvent.VK_N);
        btnNumberingConfigure.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent e) {
            Configurer config = numbering.getConfigurer();
            ConfigurerWindow window = new ConfigurerWindow(config,false);
            window.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
            window.addWindowListener(new WindowAdapter() {
              @Override
              public void windowClosed(WindowEvent evt) {
//                setNumberingFields();
              }
            });
            window.setModalityType(Dialog.ModalityType.APPLICATION_MODAL);
            window.setVisible(true);
          }
        });

        final JCheckBox chkReversed = new JCheckBox("Reversed");
        chkReversed.addItemListener(new ItemListener() {
          @Override
          public void itemStateChanged(ItemEvent e) {
            reversed = chkReversed.isSelected();
            repaint();
          }
        });

        box.add(lblBrdWidth);
        box.add(txtBrdWidth);
        box.add(txtBrdHeight);

        box.add(btnGridConfigure);
        box.add(lblGridStatus);

        box.add(btnNumberingConfigure);
        box.add(lblNumberingStatus);

        box.add(chkReversed);
        add(BorderLayout.NORTH, box);

        final JLabel lbl2 = new JLabel(
  "                                                                     "); 
        b2.add(lbl2);
        b2.add(txtLocation);
        add(BorderLayout.SOUTH,b2);
        
        brd = new Board();
        brd.setAttribute(Board.WIDTH, brdDefaultWidth);
        brd.setAttribute(Board.HEIGHT,brdDefaultHeight);

        grid = new HexGridX();
        initGrid(grid);
        grid.addTo(brd);
        pRangeStart = new Point(grid.snapTo(new Point()));
        
        numberingX =  new HexGridNumberingX();
        initNumbering(numberingX);
        numberingIrregular = new HexGridNumberingIrregular();
        initNumbering(numberingIrregular);
        
//        if (chkIrregular.isSelected()) {
          numbering = numberingX;
//        } else {
//          numbering = numberingIrregular;
//        }
        numbering.addTo(grid);

//        Toolkit tk = java.awt.Toolkit.getDefaultToolkit();
//        JOptionPane.showMessageDialog(null, tk.getScreenResolution(), "Screen Resolution", JOptionPane.INFORMATION_MESSAGE);
        
/*
        JFileChooser fc = new JFileChooser();
        fc.showSaveDialog(null);
        
        try {
          ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(fc.getSelectedFile()));
          oos.writeObject(numbering);
          oos.close();
          JOptionPane.showMessageDialog(null,"All Done!");
        } catch (FileNotFoundException e2) {
          // TODO Auto-generated catch block
          e2.printStackTrace();
        } catch (IOException e2) {
          // TODO Auto-generated catch block
          e2.printStackTrace();
        }
*/        
        JPanel p = new JPanel() {
          private static final long serialVersionUID = 1L; /* default */

          @Override
          public void paintComponent(Graphics g) {
            Rectangle r = new Rectangle(m,m, brd.getSize().width, brd.getSize().height);
            Dimension d = new Dimension(brd.getSize().width+2*m, brd.getSize().height+2*m);
            g.clearRect(0, 0, d.width, d.height);
            g.drawRect(r.x, r.y, r.width, r.height);

            grid.setOrigin(grid.getOrigin());  // How to properly fire PropertyChange events?

            grid.draw(g, r, getVisibleRect(), 1.0, reversed);
            
            this.setPreferredSize(d);

            if (pSnapTo != null && r.contains(pSnapTo)) {
              g.setColor(Color.BLACK);
              g.drawOval(pSnapTo.x-4, pSnapTo.y-4, 9,9);
              g.drawOval(pSnapTo.x,   pSnapTo.y,   1,1);
              g.setColor(Color.GRAY.brighter());
              g.drawLine(pRangeStart.x, pRangeStart.y, pSnapTo.x, pSnapTo.y);
            }

            revalidate();
          }
        };
        
        final Point pCenter = new Point();
        p.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
              Point pMouse;

              if (reversed) {
                pMouse = new Point(
                  brd.getSize().width  - (e.getX()-m),
                  brd.getSize().height - (e.getY()-m) );
              } else {
                pMouse = new Point(e.getX()-m, e.getY()-m);
              }
              try {
                pCenter.setLocation(grid.getLocation(
                    grid.locationName(pMouse)));  // new Point(pMouse)
                pRangeStart.setLocation(pCenter);
                pRangeStart.translate(m, m);

                lbl2.setText (setLbl2Text(pCenter, pMouse));
                lbl2.setSize(450, lbl2.getHeight());
              } catch (BadCoords e1) {
                e1.printStackTrace(); // Good enough for a test bed
              }
            }
          }
        );
        
        p.addMouseMotionListener(new MouseMotionAdapter(){
            @Override
            public void mouseMoved(MouseEvent e) {
              try {
                Point pMouse;
                
                if (!reversed) {
                  pMouse = new Point(e.getX()-m, e.getY()-m);
                } else if (grid.isSideways()) {
                  pMouse = new Point(
                      brd.getSize().width  + 2*grid.getOrigin().y - (e.getX()-m),
                      brd.getSize().height + 2*grid.getOrigin().x - (e.getY()-m) );
                } else {
                  pMouse = new Point(
                      brd.getSize().width  + 2*grid.getOrigin().x - (e.getX()-m),
                      brd.getSize().height + 2*grid.getOrigin().y - (e.getY()-m) );
                }
                
                
                /* Display summary on status bar */
                String s = "Mouse (" + pMouse.x  + "," + pMouse.y  + ") = ";

                String sHexMouse  = grid.locationName(pMouse); 
                Point pCentre    = grid.getLocation(sHexMouse); 
                s += " Hex-Centre (" + pCentre.x + "," + pCentre.y + ") + ";

                String sHexCentre = grid.locationName(pCentre);
                Point v = (new Point(pMouse));
                v.translate(-pCentre.x, -pCentre.y);

                s += " Hex-Centre-Offset (" + v.x + "," + v.y + ")   "
                          + sHexMouse + " / " + sHexCentre ;
                txtLocation.setText(s);

                pSnapTo    = grid.snapTo(pMouse);
                if (reversed) {
                  if (grid.isSideways()){
                    pSnapTo = new Point(
                        brd.getSize().width  + 2*grid.getOrigin().y - (pSnapTo.x),
                        brd.getSize().height + 2*grid.getOrigin().x - (pSnapTo.y) );
                  } else {
                    pSnapTo = new Point(
                      brd.getSize().width  + 2*grid.getOrigin().x - (pSnapTo.x),
                      brd.getSize().height + 2*grid.getOrigin().y - (pSnapTo.y) );
                  }
                }
                lbl2.setText (setLbl2Text(pCenter, pMouse));
                lbl2.setSize(450, lbl2.getHeight());
                pSnapTo.translate(m, m);
                
                repaint();
              } catch (BadCoords e1) {
                e1.printStackTrace();// Good enough for a test bed
              }
            }
          }
        );
        
        Dimension d = new Dimension(brdDefaultWidth+2*m, brdDefaultHeight+2*m);
        p.setPreferredSize(d);
        JScrollPane sp = new JScrollPane(p);
        add(sp,BorderLayout.CENTER);
//        setGridFields();
//        setNumberingFields();
        validate();
      }
    }
    try {
      JFrame f = new JFrame();
      f.setTitle(HexGridNumbering.class.getSimpleName() + " - Test Bed" );
      f.getContentPane().add(new TestPanel());
  //      Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
      f.setSize(new Dimension(brdDefaultWidth + 19 + 2*m,brdDefaultHeight + 85 + 2*m));
      f.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
          System.exit(0);
        }
      });
      f.setVisible(true);
    } catch (Exception e) {
      e.printStackTrace();// Good enough for a test bed
    }
  }
}

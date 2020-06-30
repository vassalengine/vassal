/*
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package VASSAL.build.module.gamepieceimage;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.Arc2D;
import java.awt.geom.GeneralPath;
import java.awt.image.BufferedImage;

import VASSAL.tools.image.ImageUtils;

public class Symbol {

  protected static final String NATO = "NATO Unit Symbols";
  protected static final String[] SYMBOL_SETS = new String[] { NATO };

  //  public static void draw(Graphics g, Rectangle r, Color fg, Color bg, String
  // symbolSet, String symbolName) {
  //
  //    if (symbolSet.equals(NATO_SIZE_SET)) {
  //
  //    }
  //    else if (symbolSet.equals(NATO_UNIT_SET)) {
  //      NatoUnitSymbolSet.draw(g, r, fg, bg, symbolName);
  //    }
  //
  //  }

  protected String symbolSetName;
  protected String symbolName1;
  protected String symbolName2;
  protected String symbolSize;

  public Symbol(String setName, String name1, String name2, String size) {
    symbolSetName = setName;
    symbolName1 = name1;
    symbolName2 = name2;
    symbolSize = size;
  }

  public void draw(Graphics g, Rectangle bounds, Color fg, Color bg, Color sz, float lineWidth) {

    if (symbolSetName.equals(NATO)) {
      NatoUnitSymbolSet.draw(symbolName1, symbolName2, g, bounds, fg, bg, sz, lineWidth, symbolSize);
    }
  }

  public static class NatoUnitSymbolSet {

    protected static final String SZ_NONE = "None";
    protected static final String SZ_INSTALLATION = "Installation";
    protected static final String SZ_TEAM = "Team";
    protected static final String SZ_SQUAD = "Squad";
    protected static final String SZ_SECTION = "Section";
    protected static final String SZ_PLATOON = "Platoon";
    protected static final String SZ_ECHELON = "Echelon";
    protected static final String SZ_COMPANY = "Company";
    protected static final String SZ_BATTALION = "Battalion";
    protected static final String SZ_REGIMENT = "Regiment";
    protected static final String SZ_BRIGADE = "Brigade";
    protected static final String SZ_DIVISION = "Division";
    protected static final String SZ_CORPS = "Corps";
    protected static final String SZ_ARMY = "Army";
    protected static final String SZ_ARMY_GROUP = "Army Group";
    protected static final String SZ_REGION = "Region";

    protected static final String NONE = "None";
    protected static final String AIRBORNE = "Airborne";
    protected static final String AIR_DEFENCE = "Air Defence";
    protected static final String AIR_FORCE = "Air Force";
    //    protected static final String AIR_MOBILE = "Air Mobile";
    //    protected static final String AMPHIBIOUS = "Amphibious";
    protected static final String ANTI_TANK = "Anti Tank";
    protected static final String ARMORED = "Armored";
    protected static final String ARMY_AVIATION = "Army Aviation";
    protected static final String ARTILLERY = "Artillery";
    //    protected static final String BRIDGING = "Bridging";
    //    protected static final String COMBAT_SERVICE_SUPPORT = "";
    protected static final String COMMANDO = "Commando";
    //    protected static final String ELECTRONIC_RANGING = "";
    //    protected static final String ELECTRONIC_WARFARE = "";
    protected static final String ENGINEERS = "Engineers";
    protected static final String GLIDER = "Glider-Borne";
    protected static final String GUERILLA = "Guerilla";
    //    protected static final String HEADQUARTERS_SUPPORT = "";
    protected static final String INFANTRY = "Infantry";
    //    protected static final String LABOR_RESOURCES = "";
    //    protected static final String MAINTENANCE = "";
    protected static final String MARINES = "Marines";
    //    protected static final String METEOROLOGICAL = "";
    //    protected static final String MILITARY_CIVIL = "";
    //    protected static final String MP = "";
    //    protected static final String MISSILE = "";
    protected static final String MOUNTAIN = "Mountain";
    protected static final String NAVY = "";
    //    protected static final String NBC = "";
    //    protected static final String ORDNANCE = "";
    //    protected static final String PARACHUTE = "";
    //    protected static final String PAY_FINANCE = "";
    //    protected static final String PERSONNEL = "";
    //    protected static final String PIPELINE = "";
    //    protected static final String POSTAL = "";
    //    protected static final String PSYCH = "";
    //    protected static final String QUARTERMASTER = "";
    protected static final String RECON = "Cavalry/Recon";

    //    protected static final String REPLACEMENT = "";
    //    protected static final String SERVICE = "";
    //    protected static final String SIGNAL = "";
    //    protected static final String SOUND_RANGING = "";
    //    protected static final String SUPPLY = "";
    //    protected static final String TRANSPORT = "";
    //    protected static final String TOPO = "";
    //    protected static final String UNMANNED_AIR = "";
    //    protected static final String VET = "";

    protected static final String INSTALLATION_SYMBOL = "m"; //$NON-NLS-1$
    protected static final String TEAM_SYMBOL = "o"; //$NON-NLS-1$
    protected static final String SQUAD_SYMBOL = "s"; //$NON-NLS-1$
    protected static final String COMPANY_SYMBOL = "i"; //$NON-NLS-1$
    protected static final String BRIGADE_SYMBOL = "x"; //$NON-NLS-1$

    protected static String[] getSymbolNames() {
      return new String[] {
        NONE,
        INFANTRY,
        RECON,
        ARMORED,
        ARTILLERY,
        ENGINEERS,
        AIRBORNE,
        AIR_DEFENCE,
        AIR_FORCE,
        ANTI_TANK,
        ARMY_AVIATION,
        COMMANDO,
        GLIDER,
        GUERILLA,
        //MARINES,
        MOUNTAIN,
        NAVY
      };
    }

    protected static String[] sizeNames;

    protected static String[] getSymbolSizes() {
      if (sizeNames == null) {
        sizeNames = new String[SIZES.length];
        for (int i = 0; i < SIZES.length; i++) {
          sizeNames[i] = SIZES[i].getName();
        }
      }
      return sizeNames;
    }

    protected static SizeOption findSize(String name) {
      for (SizeOption size : SIZES) {
        if (name.equals(size.getName())) {
          return size;
        }
      }
      return SIZES[0];
    }

    protected static final SizeOption[] SIZES = new SizeOption[] {
      new SizeOption(SZ_NONE, 0, ""), //$NON-NLS-1$
      new SizeOption(SZ_INSTALLATION, 1, INSTALLATION_SYMBOL),
      new SizeOption(SZ_TEAM, 1, TEAM_SYMBOL),
      new SizeOption(SZ_SQUAD, 1, SQUAD_SYMBOL),
      new SizeOption(SZ_SECTION, 2, SQUAD_SYMBOL),
      new SizeOption(SZ_PLATOON, 3, SQUAD_SYMBOL),
      new SizeOption(SZ_ECHELON, 4, SQUAD_SYMBOL),
      new SizeOption(SZ_COMPANY, 1, COMPANY_SYMBOL),
      new SizeOption(SZ_BATTALION, 2, COMPANY_SYMBOL),
      new SizeOption(SZ_REGIMENT, 3, COMPANY_SYMBOL),
      new SizeOption(SZ_BRIGADE, 1, BRIGADE_SYMBOL),
      new SizeOption(SZ_DIVISION, 2, BRIGADE_SYMBOL),
      new SizeOption(SZ_CORPS, 3, BRIGADE_SYMBOL),
      new SizeOption(SZ_ARMY, 4, BRIGADE_SYMBOL),
      new SizeOption(SZ_ARMY_GROUP, 5, BRIGADE_SYMBOL),
      new SizeOption(SZ_REGION, 6, BRIGADE_SYMBOL)
    };

    protected static void draw(String name1, String name2, Graphics g, Rectangle bounds, Color fg, Color bg,
        Color sz, float lineWidth, String size) {

      if (bg != null) {
        g.setColor(bg);
        g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
      }

      g.setColor(fg);
      BasicStroke stroke = new BasicStroke(lineWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
      Graphics2D g2 = ((Graphics2D) g);
      g2.setStroke(stroke);
      //g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

      g.drawRect(bounds.x, bounds.y, bounds.width, bounds.height);

      g.setColor(sz);
      drawSize(g, size, bounds);

      g.setColor(fg);
      draw(g, lineWidth, name1, bounds, false);
      draw(g, lineWidth, name2, bounds, true);
    }

    protected static void draw(Graphics g, float lineWidth, String name, Rectangle bounds, boolean drawLow) {

      Graphics2D g2 = (Graphics2D) g;
      BasicStroke stroke = new BasicStroke(lineWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
      g2.setStroke(stroke);

      int x_left = bounds.x;
      int x_center = bounds.x + bounds.width / 2 + 1;
      int x_right = bounds.x + bounds.width;

      int y_top = bounds.y;
      int y_center = bounds.y + bounds.height / 2;
      int y_bottom = bounds.y + bounds.height;

      if (name.equals(NONE)) {

      }

      else if (name.equals(AIRBORNE)) {
        int x1 = x_center - bounds.width / 4;
        int y1 = y_top + bounds.height * 4 / 5 + 1;
        g2.draw(new Arc2D.Double(x1, y1, bounds.width/4, bounds.height/4, 0,
            180, Arc2D.OPEN));
        g2.draw(new Arc2D.Double(x_center, y1, bounds.width/4, bounds.height/4, 0,
            180, Arc2D.OPEN));

      }

      else if (name.equals(AIR_DEFENCE)) {
        g2.draw(new Arc2D.Double(x_left, y_top+ bounds.height / 4, bounds.width, bounds.height*1.5, 0,
            180, Arc2D.OPEN));

      }
      else if (name.equals(AIR_FORCE)) {
        int xoff1 = (int) (bounds.width * 0.15);
        int xoff2 = (int) (bounds.width * 0.2);
        int yoff = (int) (bounds.height * 0.35);
        g.drawLine(x_center-xoff2, y_top+yoff, x_center+xoff2, y_bottom-yoff);
        g.drawLine(x_center+xoff2, y_top+yoff, x_center-xoff2, y_bottom-yoff);
        g2.draw(new Arc2D.Double(x_center-xoff2-xoff1, y_top+yoff, xoff1*2, bounds.height-(2*yoff), 90, 180, Arc2D.OPEN));
        g2.draw(new Arc2D.Double(x_center+xoff2-xoff1, y_top+yoff, xoff1*2, bounds.height-(2*yoff), 270, 180, Arc2D.OPEN));
      }

      else if (name.equals(ANTI_TANK)) {
        g.drawLine(x_left, y_bottom, x_center, y_top);
        g.drawLine(x_center, y_top, x_right, y_bottom);
      }

      else if (name.equals(ARMORED)) {
        int yoff = (int) (bounds.height * .25);
        int xoff1 = (int) (bounds.width * .15);
        int xoff2 = (int) (bounds.width * .20);
        g.drawLine(x_left+xoff1+xoff2, y_top+yoff, x_right-xoff1-xoff2, y_top+yoff);
        g.drawLine(x_left+xoff1+xoff2, y_bottom-yoff, x_right-xoff1-xoff2, y_bottom-yoff);
        g2.draw(new Arc2D.Double(x_left+xoff1, y_top+yoff, xoff2*2, bounds.height-(yoff*2), 90, 180, Arc2D.OPEN));
        g2.draw(new Arc2D.Double(x_right-xoff1-(2*xoff2), y_top+yoff, xoff2*2, bounds.height-(yoff*2), 270, 180, Arc2D.OPEN));
      }

      else if (name.equals(ARMY_AVIATION)) {
        int xoff = (int) (bounds.height * 0.25);
        int yoff = (int) (bounds.height * 0.33);
        GeneralPath p = new GeneralPath();
        p.moveTo(x_left+xoff, y_top+yoff);
        p.lineTo(x_right-yoff, y_bottom-yoff);
        p.lineTo(x_right-yoff, y_top+yoff);
        p.lineTo(x_left+xoff, y_bottom-yoff);
        p.closePath();
        g2.draw(p);
      }
      else if (name.equals(ARTILLERY)) {
        int radius = bounds.height / 5;
        int yoff = (drawLow ? (int) (bounds.height * .2) : 0);
        g.fillOval(x_center - radius, y_center - radius + yoff, radius * 2, radius * 2);
      }

      else if (name.equals(COMMANDO)) {

        g.drawLine(x_left, y_top, x_right, y_bottom);
        g.drawLine(x_left, y_bottom, x_right, y_top);
        int x1 = (int) (bounds.width / 2.5);
        int y1 = (int) (bounds.height / 2.5);

        GeneralPath p = new GeneralPath();
        p.moveTo(x_left, y_top);
        p.lineTo(x_left+x1, y_top);
        p.lineTo(x_left+x1, y_top + y1);
        p.lineTo(x_left, y_top);
        p.moveTo(x_right, y_top);
        p.lineTo(x_right-x1, y_top);
        p.lineTo(x_right-x1, y_top + y1);
        p.lineTo(x_right, y_top);
        g2.fill(p);

      }

      else if (name.equals(ENGINEERS)) {
        BasicStroke oldStroke = (BasicStroke) g2.getStroke();
        BasicStroke estroke = new BasicStroke(oldStroke.getLineWidth() * 1.2f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
        g2.setStroke(estroke);
        int yh = (int) (bounds.height * 0.2);
        int y1 = drawLow ? y_bottom - yh - 1 : y_top + (bounds.height - yh) / 2;
        int y2 = y1 + yh;
        int x1 = x_center - bounds.width / 5;
        int x2 = x_center + bounds.width / 5;

        GeneralPath p = new GeneralPath();
        p.moveTo(x1, y2);
        p.lineTo(x1, y1);
        p.lineTo(x2, y1);
        p.lineTo(x2, y2);
        p.moveTo(x_center, y2);
        p.lineTo(x_center, y1);
        g2.draw(p);
      }

      else if (name.equals(GLIDER)) {

        g.drawLine(x_left+(x_center-x_left)/3, y_center, x_right-(x_center-x_left)/3,y_center);
      }

      else if (name.equals(GUERILLA)) {

        GeneralPath p = new GeneralPath();
        p.moveTo(x_left, y_top);
        p.lineTo(x_right, y_bottom);
        p.lineTo(x_right, y_top);
        p.lineTo(x_left, y_bottom);
        p.lineTo(x_left, y_top);
        g2.fill(p);

      }

      else if (name.equals(INFANTRY)) {

        g.drawLine(x_left, y_top, x_right, y_bottom);
        g.drawLine(x_left, y_bottom, x_right, y_top);

      }

      else if (name.equals(MARINES)) {

      }

      else if (name.equals(MOUNTAIN)) {
        int x_off = bounds.width / 6;
        GeneralPath p = new GeneralPath();
        p.moveTo(x_center, y_center);
        p.lineTo(x_center+x_off, y_bottom);
        p.lineTo(x_center-x_off, y_bottom);
        p.closePath();
        g2.fill(p);
      }

      else if (name.equals(NAVY)) {
        int yoff1 = (int) (bounds.height * 0.20);
        int yoff2 = (int) (bounds.height * 0.15);
        int xoff1 = (int) (bounds.width * 0.15);
        int xoff2 = (int) (bounds.width * 0.30);
        g.drawLine(x_center, y_top+yoff1, x_center, y_bottom-yoff1);
        g.drawLine(x_center-xoff1, y_top+yoff1+yoff2, x_center+xoff1, y_top+yoff1+yoff2);
        g2.draw(new Arc2D.Double(x_center-xoff2, y_top+yoff1, xoff2*2, bounds.height-(2*yoff1), 225, 90, Arc2D.OPEN));
      }

      else if (name.equals(RECON)) {
        g.drawLine(bounds.x, bounds.y + bounds.height, bounds.x + bounds.width, bounds.y);
      }

    }

    /**
     *
     * @param g       Grahics
     * @param size    Name of size symbol
     * @param bounds  Size of the unit symbol
     */
    protected static void drawSize(Graphics g, String size, Rectangle bounds) {

      if (size.equals(SZ_NONE) || size.equals("")) { //$NON-NLS-1$
        return;
      }

      SizeOption option = findSize(size);
      String type = option.getType();
      int count = option.getCount();

      int sym_w;
      int sym_h = bounds.height / 3;
      if (count <= 4) {
        sym_w = bounds.width / 5;
      }
      else {
        sym_w = bounds.width / 7;
      }
      int gap = bounds.width / 15;

      BufferedImage bi = buildSizeImage(g, count, type, sym_w, sym_h, gap);

      int xpos = bounds.x + (bounds.width/2) - (bi.getWidth()/2) + gap; // + (gap/2) - (bi.getWidth()/2);
      int ypos = bounds.y - sym_h - 1;
      g.drawImage(bi, xpos, ypos , null);

    }

    public static BufferedImage buildSizeImage(String size, int sym_w, int sym_h, int gap) {

      SizeOption option = findSize(size);
      String type = option.getType();
      int count = option.getCount();

      BufferedImage bi = createImage(count, sym_w, sym_h, gap);
      Graphics2D g = bi.createGraphics();
      g.setBackground(null);
      g.setColor(Color.BLACK);
      return buildSizeImage(g, count, type, sym_w, sym_h, gap);
    }

    protected static BufferedImage createImage(int count, int sym_w, int sym_h, int gap) {
      int w = sym_w * count + gap * (count-1)+1;
      if (w < 1) w = sym_w;
      return ImageUtils.createCompatibleTranslucentImage(w, sym_h+1);
    }

    public static BufferedImage buildSizeImage(Graphics g, int count, String type, int sym_w, int sym_h, int gap) {

      Graphics2D g2 = (Graphics2D) g;
      BufferedImage bi;

      if (type.equals(INSTALLATION_SYMBOL)) {
        bi = createImage(count, sym_w*3, sym_h, gap);
      }
      else {
        bi = createImage(count, sym_w, sym_h, gap);
      }
      Graphics2D big = bi.createGraphics();
      big.setColor(g2.getColor());
      big.setBackground(null);

      // Force Size symbol to be drawn 1 pixel wide with anti-aliasing to ensure readability
      big.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
      big.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

      int x_pos = 0;
      for (int i = 0; i < count; i++) {
        if (type.equals(TEAM_SYMBOL)) {
          int radius = sym_w / 2;
          big.drawOval(x_pos, sym_h/3, radius * 2, radius * 2);
          big.drawLine(x_pos, sym_h, x_pos+sym_w, 0);
        }
        else if (type.equals(SQUAD_SYMBOL)) {
          int radius = sym_w / 2;
          big.fillOval(x_pos, sym_h/3, radius * 2, radius * 2);
        }
        else if (type.equals(COMPANY_SYMBOL)) {
          big.drawLine(x_pos+sym_w/2, 0, x_pos+sym_w/2, sym_h);
        }
        else if (type.equals(BRIGADE_SYMBOL)) {
          big.drawLine(x_pos, 0, x_pos+sym_w, sym_h);
          big.drawLine(x_pos, sym_h, x_pos+sym_w, 0);
        }
        else if (type.equals(INSTALLATION_SYMBOL)) {
          big.fillRect(x_pos, sym_h/2, x_pos+3*sym_w, sym_h);
        }
        x_pos += sym_w + gap;
      }

      big.dispose();

      return bi;
    }
  }

  private static class SizeOption {
    String name;
    String type;
    int count;

    public SizeOption (String n, int c, String t) {
      name = n;
      type = t;
      count = c;
    }

    public String getName() {
      return name;
    }

    public String getType() {
      return type;
    }

    public int getCount() {
      return count;
    }
  }
}

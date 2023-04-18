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

import VASSAL.i18n.Resources;
import VASSAL.tools.image.ImageUtils;

public class Symbol {

  protected static final String NATO = "NATO Unit Symbols"; //NON-NLS (no, really!)
  protected static final String[] SYMBOL_SETS = { NATO };
  protected static final String[] SYMBOL_SETS_DESC = { "Editor.Symbol.NATO" };

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
    protected static final String SZ_NONE = "None"; //NON-NLS
    protected static final String SZ_INSTALLATION = "Installation"; //NON-NLS
    protected static final String SZ_TEAM = "Team"; //NON-NLS
    protected static final String SZ_SQUAD = "Squad"; //NON-NLS
    protected static final String SZ_SECTION = "Section"; //NON-NLS
    protected static final String SZ_PLATOON = "Platoon"; //NON-NLS
    protected static final String SZ_ECHELON = "Echelon"; //NON-NLS
    protected static final String SZ_COMPANY = "Company"; //NON-NLS
    protected static final String SZ_BATTALION = "Battalion"; //NON-NLS
    protected static final String SZ_REGIMENT = "Regiment"; //NON-NLS
    protected static final String SZ_BRIGADE = "Brigade"; //NON-NLS
    protected static final String SZ_DIVISION = "Division"; //NON-NLS
    protected static final String SZ_CORPS = "Corps"; //NON-NLS
    protected static final String SZ_ARMY = "Army"; //NON-NLS
    protected static final String SZ_ARMY_GROUP = "Army Group"; //NON-NLS //NON-NLS
    protected static final String SZ_REGION = "Region";

    protected static final String NONE = "None"; //NON-NLS
    protected static final String AIRBORNE = "Airborne"; //NON-NLS
    protected static final String AIR_DEFENCE = "Air Defence"; //NON-NLS
    protected static final String AIR_FORCE = "Air Force"; //NON-NLS
    protected static final String ANTI_TANK = "Anti Tank"; //NON-NLS
    protected static final String ARMORED = "Armored"; //NON-NLS
    protected static final String ARMY_AVIATION = "Army Aviation"; //NON-NLS
    protected static final String ARTILLERY = "Artillery"; //NON-NLS
    protected static final String COMMANDO = "Commando"; //NON-NLS
    protected static final String ENGINEERS = "Engineers"; //NON-NLS
    protected static final String GLIDER = "Glider-Borne"; //NON-NLS
    protected static final String GUERILLA = "Guerrilla"; //NON-NLS
    protected static final String INFANTRY = "Infantry"; //NON-NLS
    protected static final String MARINES = "Marines"; //NON-NLS
    protected static final String MOUNTAIN = "Mountain"; //NON-NLS
    protected static final String NAVY = ""; //NON-NLS
    protected static final String RECON = "Cavalry/Recon"; //NON-NLS

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
        MOUNTAIN,
        NAVY
      };
    }

    protected static String[] getSymbolDisplayNames() {
      return new String[] {
        "Editor.Symbol.none",
        "Editor.Symbol.infantry",
        "Editor.Symbol.cavalry_recon",
        "Editor.Symbol.armored",
        "Editor.Symbol.artillery",
        "Editor.Symbol.engineers",
        "Editor.Symbol.airborne",
        "Editor.Symbol.air_defense",
        "Editor.Symbol.air_force",
        "Editor.Symbol.anti_tank",
        "Editor.Symbol.army_aviation",
        "Editor.Symbol.commando",
        "Editor.Symbol.glider_borne",
        "Editor.Symbol.guerrilla",
        "Editor.Symbol.mountain",
        "Editor.Symbol.navy"
      };
    }

    protected static String[] sizeNames;
    protected static String[] sizeDisplayNames;

    protected static String[] getSymbolSizes() {
      if (sizeNames == null) {
        sizeNames = new String[SIZES.length];
        for (int i = 0; i < SIZES.length; i++) {
          sizeNames[i] = SIZES[i].getName();
        }
      }
      return sizeNames;
    }

    protected static String[] getSymbolSizeDisplayNames() {
      if (sizeDisplayNames == null) {
        sizeDisplayNames = new String[SIZES.length];
        for (int i = 0; i < SIZES.length; i++) {
          sizeDisplayNames[i] = SIZES[i].getDisplayName();
        }
      }
      return sizeDisplayNames;
    }

    protected static SizeOption findSize(String name) {
      for (final SizeOption size : SIZES) {
        if (name.equals(size.getName())) {
          return size;
        }
      }
      return SIZES[0];
    }

    protected static SizeOption findSizeByDisplayName(String displayName) {
      for (final SizeOption size : SIZES) {
        if (displayName.equals(size.getDisplayName())) {
          return size;
        }
      }
      return SIZES[0];
    }

    public static final SizeOption[] SIZES = {
      new SizeOption(SZ_NONE, Resources.getString("Editor.Symbol.Size.none"), 0, ""), //$NON-NLS-1$
      new SizeOption(SZ_INSTALLATION, Resources.getString("Editor.Symbol.Size.installation"), 1, INSTALLATION_SYMBOL),
      new SizeOption(SZ_TEAM, Resources.getString("Editor.Symbol.Size.team"), 1, TEAM_SYMBOL),
      new SizeOption(SZ_SQUAD, Resources.getString("Editor.Symbol.Size.squad"), 1, SQUAD_SYMBOL),
      // Note: Platoon and Section constants reversed due to earlier error. Maintain this switch for compatibility
      new SizeOption(SZ_PLATOON, Resources.getString("Editor.Symbol.Size.section"), 2, SQUAD_SYMBOL),
      new SizeOption(SZ_SECTION, Resources.getString("Editor.Symbol.Size.platoon"), 3, SQUAD_SYMBOL),
      new SizeOption(SZ_ECHELON, Resources.getString("Editor.Symbol.Size.echelon"), 4, SQUAD_SYMBOL),
      new SizeOption(SZ_COMPANY, Resources.getString("Editor.Symbol.Size.company"), 1, COMPANY_SYMBOL),
      new SizeOption(SZ_BATTALION, Resources.getString("Editor.Symbol.Size.battalion"), 2, COMPANY_SYMBOL),
      new SizeOption(SZ_REGIMENT, Resources.getString("Editor.Symbol.Size.regiment"), 3, COMPANY_SYMBOL),
      new SizeOption(SZ_BRIGADE, Resources.getString("Editor.Symbol.Size.brigade"), 1, BRIGADE_SYMBOL),
      new SizeOption(SZ_DIVISION, Resources.getString("Editor.Symbol.Size.division"), 2, BRIGADE_SYMBOL),
      new SizeOption(SZ_CORPS, Resources.getString("Editor.Symbol.Size.corps"), 3, BRIGADE_SYMBOL),
      new SizeOption(SZ_ARMY, Resources.getString("Editor.Symbol.Size.army"), 4, BRIGADE_SYMBOL),
      new SizeOption(SZ_ARMY_GROUP, Resources.getString("Editor.Symbol.Size.army_group"), 5, BRIGADE_SYMBOL),
      new SizeOption(SZ_REGION, Resources.getString("Editor.Symbol.Size.region"), 6, BRIGADE_SYMBOL)
    };

    protected static void draw(String name1, String name2, Graphics g, Rectangle bounds, Color fg, Color bg,
        Color sz, float lineWidth, String size) {

      if (bg != null) {
        g.setColor(bg);
        g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
      }

      g.setColor(fg);
      final BasicStroke stroke = new BasicStroke(lineWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
      final Graphics2D g2 = ((Graphics2D) g);
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
      if (name.equals(NONE) || name.equals(MARINES)) {
        return;
      }

      final Graphics2D g2 = (Graphics2D) g;
      final BasicStroke stroke = new BasicStroke(lineWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
      g2.setStroke(stroke);

      final int x_left = bounds.x;
      final int x_center = bounds.x + bounds.width / 2 + 1;
      final int x_right = bounds.x + bounds.width;

      final int y_top = bounds.y;
      final int y_center = bounds.y + bounds.height / 2;
      final int y_bottom = bounds.y + bounds.height;

      switch (name) {
      case AIRBORNE: {
        final int x1 = x_center - bounds.width / 4;
        final int y1 = y_top + bounds.height * 4 / 5 + 1;
        g2.draw(new Arc2D.Double(x1, y1, bounds.width / 4, bounds.height / 4, 0,
          180, Arc2D.OPEN));
        g2.draw(new Arc2D.Double(x_center, y1, bounds.width / 4, bounds.height / 4, 0,
          180, Arc2D.OPEN));

        break;
      }

      case AIR_DEFENCE:
        g2.draw(new Arc2D.Double(x_left, y_top + bounds.height / 4, bounds.width, bounds.height * 1.5, 0,
          180, Arc2D.OPEN));
        break;

      case AIR_FORCE: {
        final int xoff1 = (int) (bounds.width * 0.15);
        final int xoff2 = (int) (bounds.width * 0.2);
        final int yoff = (int) (bounds.height * 0.35);
        g.drawLine(x_center - xoff2, y_top + yoff, x_center + xoff2, y_bottom - yoff);
        g.drawLine(x_center + xoff2, y_top + yoff, x_center - xoff2, y_bottom - yoff);
        g2.draw(new Arc2D.Double(x_center - xoff2 - xoff1, y_top + yoff, xoff1 * 2, bounds.height - (2 * yoff), 90, 180,
          Arc2D.OPEN));
        g2.draw(
          new Arc2D.Double(x_center + xoff2 - xoff1, y_top + yoff, xoff1 * 2, bounds.height - (2 * yoff), 270, 180,
            Arc2D.OPEN));
        break;
      }

      case ANTI_TANK:
        g.drawLine(x_left, y_bottom, x_center, y_top);
        g.drawLine(x_center, y_top, x_right, y_bottom);
        break;

      case ARMORED: {
        final int yoff = (int) (bounds.height * .25);
        final int xoff1 = (int) (bounds.width * .15);
        final int xoff2 = (int) (bounds.width * .20);
        g.drawLine(x_left + xoff1 + xoff2, y_top + yoff, x_right - xoff1 - xoff2, y_top + yoff);
        g.drawLine(x_left + xoff1 + xoff2, y_bottom - yoff, x_right - xoff1 - xoff2, y_bottom - yoff);
        g2.draw(
          new Arc2D.Double(x_left + xoff1, y_top + yoff, xoff2 * 2, bounds.height - (yoff * 2), 90, 180, Arc2D.OPEN));
        g2.draw(
          new Arc2D.Double(x_right - xoff1 - (2 * xoff2), y_top + yoff, xoff2 * 2, bounds.height - (yoff * 2), 270, 180,
            Arc2D.OPEN));
        break;
      }

      case ARMY_AVIATION: {
        final int xoff = (int) (bounds.height * 0.25);
        final int yoff = (int) (bounds.height * 0.33);
        final GeneralPath p = new GeneralPath();
        p.moveTo(x_left + xoff, y_top + yoff);
        p.lineTo(x_right - yoff, y_bottom - yoff);
        p.lineTo(x_right - yoff, y_top + yoff);
        p.lineTo(x_left + xoff, y_bottom - yoff);
        p.closePath();
        g2.draw(p);
        break;
      }

      case ARTILLERY: {
        final int radius = bounds.height / 5;
        final int yoff = (drawLow ? (int) (bounds.height * .2) : 0);
        g.fillOval(x_center - radius, y_center - radius + yoff, radius * 2, radius * 2);
        break;
      }

      case COMMANDO: {
        g.drawLine(x_left, y_top, x_right, y_bottom);
        g.drawLine(x_left, y_bottom, x_right, y_top);
        final int x1 = (int) (bounds.width / 2.5);
        final int y1 = (int) (bounds.height / 2.5);

        final GeneralPath p = new GeneralPath();
        p.moveTo(x_left, y_top);
        p.lineTo(x_left + x1, y_top);
        p.lineTo(x_left + x1, y_top + y1);
        p.lineTo(x_left, y_top);
        p.moveTo(x_right, y_top);
        p.lineTo(x_right - x1, y_top);
        p.lineTo(x_right - x1, y_top + y1);
        p.lineTo(x_right, y_top);
        g2.fill(p);

        break;
      }

      case ENGINEERS: {
        final BasicStroke oldStroke = (BasicStroke) g2.getStroke();
        final BasicStroke estroke =
          new BasicStroke(oldStroke.getLineWidth() * 1.2f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
        g2.setStroke(estroke);
        final int yh = (int) (bounds.height * 0.2);
        final int y1 = drawLow ? y_bottom - yh - 1 : y_top + (bounds.height - yh) / 2;
        final int y2 = y1 + yh;
        final int x1 = x_center - bounds.width / 5;
        final int x2 = x_center + bounds.width / 5;

        final GeneralPath p = new GeneralPath();
        p.moveTo(x1, y2);
        p.lineTo(x1, y1);
        p.lineTo(x2, y1);
        p.lineTo(x2, y2);
        p.moveTo(x_center, y2);
        p.lineTo(x_center, y1);
        g2.draw(p);
        break;
      }
      case GLIDER:

        g.drawLine(x_left + (x_center - x_left) / 3, y_center, x_right - (x_center - x_left) / 3, y_center);
        break;
      case GUERILLA: {

        final GeneralPath p = new GeneralPath();
        p.moveTo(x_left, y_top);
        p.lineTo(x_right, y_bottom);
        p.lineTo(x_right, y_top);
        p.lineTo(x_left, y_bottom);
        p.lineTo(x_left, y_top);
        g2.fill(p);

        break;
      }
      case INFANTRY:

        g.drawLine(x_left, y_top, x_right, y_bottom);
        g.drawLine(x_left, y_bottom, x_right, y_top);

        break;
      case MOUNTAIN: {
        final int x_off = bounds.width / 6;
        final GeneralPath p = new GeneralPath();
        p.moveTo(x_center, y_center);
        p.lineTo(x_center + x_off, y_bottom);
        p.lineTo(x_center - x_off, y_bottom);
        p.closePath();
        g2.fill(p);
        break;
      }
      case NAVY: {
        final int yoff1 = (int) (bounds.height * 0.20);
        final int yoff2 = (int) (bounds.height * 0.15);
        final int xoff1 = (int) (bounds.width * 0.15);
        final int xoff2 = (int) (bounds.width * 0.30);
        g.drawLine(x_center, y_top + yoff1, x_center, y_bottom - yoff1);
        g.drawLine(x_center - xoff1, y_top + yoff1 + yoff2, x_center + xoff1, y_top + yoff1 + yoff2);
        g2.draw(new Arc2D.Double(x_center - xoff2, y_top + yoff1, xoff2 * 2, bounds.height - (2 * yoff1), 225, 90,
          Arc2D.OPEN));
        break;
      }
      case RECON:
        g.drawLine(bounds.x, bounds.y + bounds.height, bounds.x + bounds.width, bounds.y);
        break;
      default:
        // do nothing
      }
    }

    /**
     *
     * @param g       Graphics
     * @param size    Name of size symbol
     * @param bounds  Size of the unit symbol
     */
    protected static void drawSize(Graphics g, String size, Rectangle bounds) {

      if (size.equals(SZ_NONE) || size.equals("")) { //$NON-NLS-1$
        return;
      }

      final SizeOption option = findSize(size);
      final String type = option.getType();
      final int count = option.getCount();

      final int sym_w;
      final int sym_h = bounds.height / 3;
      if (count <= 4) {
        sym_w = bounds.width / 5;
      }
      else {
        sym_w = bounds.width / 7;
      }
      final int gap = bounds.width / 15;

      final BufferedImage bi = buildSizeImage(g, count, type, sym_w, sym_h, gap);

      final int xpos = bounds.x + (bounds.width / 2) - (bi.getWidth() / 2) + gap; // + (gap/2) - (bi.getWidth()/2);
      final int ypos = bounds.y - sym_h - 1;
      g.drawImage(bi, xpos, ypos, null);
    }

    public static BufferedImage buildSizeImage(String size, int sym_w, int sym_h, int gap) {

      final SizeOption option = findSize(size);
      final String type = option.getType();
      final int count = option.getCount();

      final BufferedImage bi = createImage(count, sym_w, sym_h, gap);
      final Graphics2D g = bi.createGraphics();
      g.setBackground(null);
      g.setColor(Color.BLACK);
      return buildSizeImage(g, count, type, sym_w, sym_h, gap);
    }

    protected static BufferedImage createImage(int count, int sym_w, int sym_h, int gap) {
      int w = sym_w * count + gap * (count - 1) + 1;
      if (w < 1) w = sym_w;
      return ImageUtils.createCompatibleTranslucentImage(w, sym_h + 1);
    }

    public static BufferedImage buildSizeImage(Graphics g, int count, String type, int sym_w, int sym_h, int gap) {

      final Graphics2D g2 = (Graphics2D) g;
      final BufferedImage bi;

      if (type.equals(INSTALLATION_SYMBOL)) {
        bi = createImage(count, sym_w * 3, sym_h, gap);
      }
      else {
        bi = createImage(count, sym_w, sym_h, gap);
      }
      final Graphics2D big = bi.createGraphics();
      big.setColor(g2.getColor());
      big.setBackground(null);

      // Force Size symbol to be drawn 1 pixel wide with anti-aliasing to ensure readability
      big.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
      big.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

      int x_pos = 0;
      for (int i = 0; i < count; i++) {
        if (type.equals(TEAM_SYMBOL)) {
          final int radius = sym_w / 2;
          big.drawOval(x_pos, sym_h / 3, radius * 2, radius * 2);
          big.drawLine(x_pos, sym_h, x_pos + sym_w, 0);
        }
        else if (type.equals(SQUAD_SYMBOL)) {
          final int radius = sym_w / 2;
          big.fillOval(x_pos, sym_h / 3, radius * 2, radius * 2);
        }
        else if (type.equals(COMPANY_SYMBOL)) {
          big.drawLine(x_pos + sym_w / 2, 0, x_pos + sym_w / 2, sym_h);
        }
        else if (type.equals(BRIGADE_SYMBOL)) {
          big.drawLine(x_pos, 0, x_pos + sym_w, sym_h);
          big.drawLine(x_pos, sym_h, x_pos + sym_w, 0);
        }
        else if (type.equals(INSTALLATION_SYMBOL)) {
          big.fillRect(x_pos, sym_h / 2, x_pos + 3 * sym_w, sym_h);
        }
        x_pos += sym_w + gap;
      }

      big.dispose();

      return bi;
    }
  }

  public static class SizeOption {
    String name;
    String displayName;
    String type;
    int count;

    public SizeOption(String n, String d, int c, String t) {
      name = n;
      displayName = d;
      type = t;
      count = c;
    }

    public String getName() {
      return name;
    }

    public String getDisplayName() {
      return displayName;
    }

    public String getType() {
      return type;
    }

    public int getCount() {
      return count;
    }
  }
}

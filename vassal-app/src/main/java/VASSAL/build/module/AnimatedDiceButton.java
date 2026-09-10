/*
 *
 * Copyright (c) 2025 Christian Holm Christensen
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
package VASSAL.build.module;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JRootPane;
import javax.swing.JLabel;
import javax.swing.UIManager;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.awt.event.MouseAdapter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Collections;
    
/**
 * A button that rolls a animated dice.  This extends
 * SpecialDiceButton and overrides only the relevant parts.
 */
public class AnimatedDiceButton extends SpecialDiceButton  {
  private static final Logger logger =
    LoggerFactory.getLogger(AnimatedDiceButton.class);

  protected JDialog           dialog; // Dialog to show results graphical
  protected JLabel            dialogLabel;
  protected int               steps             = 4;
  protected int               fps               = 30;
  protected int               lastTotal         = 0;
  protected AnimResultsIcon   animResultsIcon   = new AnimResultsIcon();
  protected ButtonResultsIcon buttonResultsIcon = new ButtonResultsIcon();
  protected Thread            animator;
  protected int               delay             = 1000 / fps;
  protected double            buttonScale       = .5;
    
  public static final String STEPS        = "steps"; //$NON-NLS-1$
  public static final String FPS          = "fps"; //$NON-NLS-1$
  public static final String BUTTON_SCALE = "buttonScale"; //$NON-NLS-1$
  private static final int   BUTTON_OFF   = 4;

  public AnimatedDiceButton() {
    super(); 
    
    dialog = new JDialog(GameModule.getGameModule().getPlayerWindow());
    
    // Below will make the frame transparent but still have window
    // decorations, etc., but only with the default Metal
    // look'n'feel. Other look'n'feel does not render this
    // correctly - the window will have no decorations, and in
    // some cases a filled background.
    if (UIManager.getLookAndFeel().getName().equals("Metal")) { //$NON-NLS-1$
      dialog.setUndecorated(true);
      dialog.getRootPane().setWindowDecorationStyle(JRootPane.FRAME);
      dialog.setBackground(new Color(0, 0, 0, 0));
    }
    
    // Make label to display the dice 
    dialogLabel = new JLabel();
    dialogLabel.setIcon(animResultsIcon);
    dialogLabel.setBackground(new Color(0, 0, 0, 0));
    dialogLabel.addMouseListener(new MouseAdapter() {
        // When clicking the window, it will hide itself.
        @Override
        public void mouseClicked(MouseEvent e) {
          dialog.setVisible(false);
        }
      });        
    dialog.setContentPane(dialogLabel);
  }

  /**
   * Forwards the result of the roll to the {@link Chatter#send}
   * method of the {@link Chatter} of the {@link GameModule}.  Format
   * is prefix+[comma-separated roll list]+suffix additionally a
   * command for every die is generated
   *
   * Note that we calculate the total here so that it can be set
   * earlier then in the super class (not done yet). This triggers the
   * animation and reporting of the results.
   */
  @Override
  protected void DR() {
    final int[] results = new int[dice.size()];
    int i = 0;
    lastTotal = 0;
    for (final SpecialDie sd : dice) {
      final int faceCount = sd.getFaceCount();
      final int roll  =  faceCount == 0 ? 0 : ran.nextInt(sd.getFaceCount());
      final int value =  sd.getIntValue(roll);
      results[i++]    =  roll;            
      lastTotal       += value;
    }
    setFormat(results);
    reportAnimResults(results);
  }

  private void actionsAndSend(Command c) {
    try {
      doActions();
    }
    catch (RecursionLimitException ex) {
      RecursionLimiter.infiniteLoop(ex);
    }
    
    GameModule.getGameModule().sendAndLog(c);
  }

  private class WindowThread implements Runnable {
    public Command command;
    /**
     * Thread function to draw the result of the die roll as an animation.
     */
    @Override
    public void run() {
      long         start   = System.currentTimeMillis();
      final Thread current = Thread.currentThread();
      int          step    = 0;
      
      while (current == animator && step < steps + 1) {
        try {
          start += delay;
          // Delay next frame depending on how far behind current time
          // we are.  For simple animations, incrementing start by the
          // delay value will probably keep pace with the current time,
          // but if we're doing a lot in this while loop, we'll
          // eventually fall behind current time.  So to keep up,
          // sleep(0) will be used. Also, see the if statement below.
          Thread.sleep(Math.max(0, start - System.currentTimeMillis()));
        }
        catch (InterruptedException e) {
          logger.error("An error occurred: " + e.toString());
        }
        
        // Paint current step
        // logger.info(String.format("Painting inner step # %d / %d",
        //                           step, steps));
        
        animResultsIcon.setStep(step++);
        dialogLabel.repaint();
        
        // Figure out how fast it is running; if start is more than 1000
        // ms behind current time, reset start to current time
        if (start + 1000 < System.currentTimeMillis()) {
          start = System.currentTimeMillis();
        }
      }
      // Done at end of thread
      if (reportResultInButton) {
        buttonResultsIcon.setWaiting(false);
        getLaunchButton().setIcon(buttonResultsIcon);
        getLaunchButton().repaint();
      }
      if (reportResultAsText) {
        command = command.append(reportAnimTextResults());
      }
      actionsAndSend(command);
    }
  }

  private final WindowThread windowThread = new WindowThread();
  
  /**
   * Show the result of the die roll graphically as a pop-up window
   * or on the launch button (or both).  Note, the drawing of the
   * pop-up is done in a thread that loops over the `steps`
   * intermediate icons and ends in the icons corresponding to the
   * die roll.
   */
  private void reportAnimResults(int[] results) {
    animResultsIcon.setResults(results);
    buttonResultsIcon.setResults(results);

    final Command c = new ShowResults(this, results);
    
    if (reportResultInWindow) {
      final int w = animResultsIcon.getIconWidth();
      final int h = animResultsIcon.getIconHeight();
      dialogLabel.setSize(new Dimension(w, h));
      dialogLabel.setMinimumSize(new Dimension(w, h));
      format.setFormat(windowTitleResultFormat);
      dialog.setTitle(format.getLocalizedText(this, "Editor.SpecialDiceButton.window_title"));
      dialog.pack();
      dialog.setVisible(true);

      buttonResultsIcon.setWaiting(true);

      windowThread.command = c;
      animator = new Thread(windowThread);
      animator.start();
    }
    else {
      // Remove dialog if not enabled
      dialog.setVisible(false);
    }
    if (reportResultInButton) {
      if (!reportResultInWindow) {
        // Ensure redraw, and our icon
        buttonResultsIcon.setWaiting(false);
        getLaunchButton().setIcon(buttonResultsIcon);
        getLaunchButton().repaint();    
      }
    }
    else {
      // Remove icon if not enabled
      getLaunchButton().setIcon(null);
      getLaunchButton().repaint();    
    }
    getLaunchButton().repaint();    
    if (reportResultAsText && !reportResultInWindow) {
      c.append(reportAnimTextResults());
    }
    actionsAndSend(c);
  }

    
  /**
   * Formats the result as text according to developer settings.
   *
   * Also sets the property `result`.
   */
  private Command reportAnimTextResults() {
    format.setFormat(chatResultFormat);
    String msg = format.getLocalizedText(this, "Editor.report_format");
    if (msg.length() > 0) {
      if (msg.startsWith("*")) { //$NON-NLS-1$
        msg = "*" + msg; //$NON-NLS-1$
      }
      else {
        msg = "* " + msg; //$NON-NLS-1$
      }
    }
    final Command c = msg.length() == 0 ?
      new NullCommand() :
      new Chatter.DisplayText(GameModule.getGameModule().getChatter(), msg);
    c.execute();
    // This is kinda late - the property `<Name>_result` will not
    // be set until _after_ the die roll has completed.  Perhaps
    // it should really be dine before.
    c.append(property.setPropertyValue(String.valueOf(lastTotal)));
    return c;
  }

  /**
   * Overriden so we may leverage the previously calculated total,
   * also, it executes a command to set various properties.  This is
   * done here, so that those properties are available to actions
   * and the reporting format.
   */
  @Override
  protected void setFormat(int[] results) {
    // Command  c = property.setPropertyValue(String.valueOf(lastTotal));
            
    format.setProperty(NAME, getLocalizedConfigureName());
    String full = "";
    for (int i = 0; i < dice.size() && i < results.length; ++i) {
      final SpecialDie die       = dice.get(i);
      final String     dieResult = die.getTextValue(results[i]);
      format.setProperty("result" + (i + 1), dieResult); //$NON-NLS-1$
      full += " " + dieResult;
    }
    format.setProperty(RESULT_TOTAL, String.valueOf(lastTotal)); //$NON-NLS-1$
    format.setProperty("fullResult", full); //$NON-NLS-1$
    format.setFormat(chatResultFormat);

    //GameModule.getGameModule().sendAndLog(c);        
  }
    
  /**
   * The Attributes of a AnimatedDiceButton same as SpecialDiceButton,
   * plus
   *
   * <code>STEPS</code> the number of steps taken in the animation of
   * the result.
   */
  @Override
  public String[] getAttributeNames() {
    String[] names = super.getAttributeNames();
    names = ArrayUtils.remove(names, names.length - BUTTON_OFF);
    return ArrayUtils.addAll(names, STEPS, FPS, RESULT_BUTTON, BUTTON_SCALE);
  }

  /**
   * Overriden to give descriptions of added attributes STEPS and
   * FPS.  Note, this should really use the Locale service to get
   * localised descriptions.
   */
  @Override
  public String[] getAttributeDescriptions() {
    String[] descs = super.getAttributeDescriptions();
    descs = ArrayUtils.remove(descs, descs.length - BUTTON_OFF);
    return ArrayUtils.addAll(descs,
                             Resources.getString("Editor.AnimatedDiceButton.steps"),
                             Resources.getString("Editor.AnimatedDiceButton.fps"),
                             Resources.getString("Editor.SpecialDiceButton.result_button"),
                             Resources.getString("Editor.AnimatedDiceButton.button_scale")
                             );
  }
  /**
   * Overriden to give types of added attributes STEPS and FPS.
   */
  @Override
  public Class<?>[] getAttributeTypes() {
    Class<?>[] clss = super.getAttributeTypes();
    clss = ArrayUtils.remove(clss, clss.length - BUTTON_OFF);
    return ArrayUtils.addAll(clss,
                             Integer.class,
                             Integer.class,
                             Boolean.class,
                             Double.class);
  }

  /**
   * Override to disable some configurer elements based on choices
   */
  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (List.of(WINDOW_X, WINDOW_Y, BACKGROUND_COLOR, STEPS, FPS).contains(name))
      return () -> reportResultInWindow;
    if (BUTTON_SCALE.equals(name))
      return () -> reportResultInButton;
    return super.getAttributeVisibility(name);
  }

  /**
   * The name to show in editor
   */
  public static String getConfigureTypeName() {
    return Resources.getString("Editor.AnimatedDiceButton.component_type"); //$NON-NLS-1$
  }
  


  /**
   * Set attribute key to value o.  Overriden for added attributes
   * STEPS and FPS, as well as setting our launch icon. 
   */
  @Override
  public void setAttribute(String key, Object o) {
    if (STEPS.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      steps = (Integer) o;
    }
    if (FPS.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      fps = (Integer) o;
      delay = 1000 / fps;
    }
    if (BUTTON_SCALE.equals(key)) {
      if (o instanceof String) {
        o = Double.valueOf((String) o);
      }
      buttonScale = (Double) o;
    }
    else if (RESULT_BUTTON.equals(key)) {
      // When we get the result on button configure, we make
      // sure we attach our result icon (not the one from the
      // super-class) to the button, so that that get's
      // displayed.
      reportResultInButton = Boolean.TRUE.equals(o) || "true".equals(o);
      if (reportResultInButton) {
        if (getLaunchButton() != null) {
          getLaunchButton().setIcon(buttonResultsIcon);
        }
      }
    }
    else if (WINDOW_X.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      animResultsIcon.width = (Integer) o;
      buttonResultsIcon.width = (Integer) o;
      dialog.pack();
    }
    else if (WINDOW_Y.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      animResultsIcon.height = (Integer) o;
      buttonResultsIcon.height = (Integer) o;
      dialog.pack();
    }
    else {
      super.setAttribute(key, o);
    }
  }
    
  @Override
  public String getAttributeValueString(String key) {
    if (STEPS.equals(key)) {
      return String.valueOf(steps);
    }
    if (FPS.equals(key)) {
      return String.valueOf(fps);
    }
    if (BUTTON_SCALE.equals(key)) {
      return String.valueOf(buttonScale);
    }
    else if (WINDOW_X.equals(key)) {
      return String.valueOf(animResultsIcon.width);
    }
    else if (WINDOW_Y.equals(key)) {
      return String.valueOf(animResultsIcon.height);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  /**
   * Override to set icon on launch button to our results
   * icon. Could be simplified if super class' ResultIcon was
   * protected.
   */
  @Override
  public void addTo(Buildable parent) {
    super.addTo(parent);
        
    animResultsIcon.setResults(new int[dice.size()]);
    buttonResultsIcon.setResults(new int[dice.size()]);
        
    final LaunchButton lb = getLaunchButton();
    if (lb != null && reportResultInButton) {
      lb.setIcon(buttonResultsIcon);
    }
  }
        

  /**
   * Encode a command to set the result.
   *
   * Could perhaps be avoid if `reportResult` and `reportTextResult`
   * where protected rather than private.
   */
  @Override
  public String encode(Command c) {
    if (!(c instanceof ShowAnimResults)) {
      return null;
    }
    final ShowAnimResults c2 = (ShowAnimResults) c;
    final SequenceEncoder se = new SequenceEncoder(c2.target.getIdentifier(), '\t');
    for (int i = 0; i < c2.rolls.length; ++i) {
      se.append(Integer.toString(c2.rolls[i])); //$NON-NLS-1$
    }
    return SHOW_RESULTS_COMMAND + se.getValue();
  }

  /**
   * Decode a command that sets the result.
   *
   * Could perhaps be avoid if `reportResult` and `reportTextResult`
   * where protected rather than private.
   */
  @Override
  public Command decode(String s) {
    SequenceEncoder.Decoder st = null;
    if (s.startsWith(SHOW_RESULTS_COMMAND + getConfigureName()) ||
        s.startsWith(SHOW_RESULTS_COMMAND + getId())) {
      st = new SequenceEncoder.Decoder(s, '\t');
      st.nextToken();
      st.nextToken();
    }
    else if (s.startsWith(getId() + '\t')) { // Backward compatibility
      st = new SequenceEncoder.Decoder(s, '\t');
      st.nextToken();
    }
    if (st == null) {
      return null;
    }
    final List<String> l = new ArrayList<>();
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    final int[] results = new int[l.size()];
    int i = 0;
    for (final String n : l) {
      results[i++] = Integer.parseInt(n);
    }
    return new ShowAnimResults(this, results);
  }

  /**
   * Command for displaying the results of a roll of the dice.
   *
   * Could perhaps be avoid if `reportResult` and `reportTextResult`
   * where protected rather than private.
   */
  public static class ShowAnimResults extends Command {
    private final AnimatedDiceButton target;
    private final int[] rolls;
        
    public ShowAnimResults(AnimatedDiceButton oTarget, int[] results) {
      target = oTarget;
      rolls = Arrays.copyOf(results, results.length);
    }

    @Override
    protected void executeCommand() {
      target.setFormat(rolls);
      target.reportAnimResults(rolls);
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }

  /** Base for result icons */
  private abstract class BaseResultsIcon implements Icon {
    protected int width = 25, height = 25;
    protected ImageOp[] todraw;
    protected int[] widths;
    protected int[] heights;
        
    public BaseResultsIcon() {}

    /**
     * Get a "result" image - or more correctly - face image.  The
     * image may have been cached. May return null.
     */
    protected ImageOp getFaceImage(int diceNo, int face) {
      if (diceNo >= dice.size()) return null;
            
      final String imageName = dice.get(diceNo).getImageName(face);

      if (imageName.length() <= 0) return null;
            
      final ImageOp sop = Op.load(imageName);
            
      if (sop.getImage() == null) return null;
            
      return sop;
    }
    /**
     * Get the (cached) result icon for a specific die and
     * result. May return null.
     */
    protected ImageOp getResultIcon(int diceNo, int result) {
      final ImageOp img = getFaceImage(diceNo, result);
            
      if (img.getImage() == null) return null;
            
      return img;
    }

    /**
     * Rotate image by an arbitrary angle
     */
    protected ImageOp rotateImage(ImageOp image) {
      final int    ang = ran.nextInt(360);
      return Op.rotate(image, ang);
    }
        
    /**
     * Scale an icon
     */
    protected ImageOp scaleIcon(ImageOp orig, double scale) {
      return Op.scale(orig, scale);
    }

    /** Calls implementation */
    protected void setResults(int[] results) {
      todraw  = new ImageOp[results.length];
      widths  = new int[results.length];
      heights = new int[results.length];
      if (results.length > dice.size()) {
        logger.warn(
                    "Animated Die Button (" +
                    getConfigureName() + //NON-NLS
                    "): more results (" +
                    results.length +
                    ") requested than dice (" + //NON-NLS
                    dice.size() + ")" //NON-NLS
                    );
      }
      doResults(results);
    }
    /** To be implemented */
    protected abstract void doResults(int[] results);
        
    /**
     * Paint the icon.  This relies on the current set of icons being
     * set in `todraw`.
     */
    protected void doPaint(Component c,
                           Graphics g,
                           int x,
                           int y,
                           boolean scale,
                           Color bg) {
      // Take last row of icons if not in animation
      if (todraw == null) {
        logger.warn("Nothing to draw!");
        return;
      }
            
      final int h  = getIconHeight();
      if (bg != null) {
        final int w  = getIconWidth();
        g.setColor(bg);
        g.fillRect(x, y, w, h);
      }

      int offset = 0;

      for (int i = 0; i < todraw.length; i++) {
        final ImageOp icon = todraw[i];
        if (icon != null) {
          final ImageOp ic = (scale ? scaleIcon(icon, buttonScale) : icon);
          final int dy = (int)((h - ic.getHeight()) / 2);
          final int dx = (int)((widths[i] - ic.getWidth()) / 2);
          new ImageIcon(ic.getImage()).paintIcon(c, g,
                                                 x + offset + dx,
                                                 y + dy);
          offset += widths[i];
        }
      }
    }
        
    /**
     * Get the required width of the icon by investigating the sum
     * width of all the images.
     */
    @Override
    public abstract int getIconWidth();

    /**
     * Get the required height of the icon by investigating the
     * maximum height of all the images.
     */
    @Override
    public abstract int getIconHeight();
  };

  /** Icon class for button dusplay of dice roll */
  private class ButtonResultsIcon extends BaseResultsIcon {
    boolean waiting = false;
    
    /**
     * Set the results of the die-rolls.
     *
     * This samples the die faces - except the result face - for
     * `steps` images, and builds a table of `steps+1` times N-dice
     * (`steps+1` rows, N-dice columns) icons, which we can then
     * iterate over when animating.  The last row is the result.
     * Note, intermediate images (the first `steps` rows) are randomly
     * rotated to create an effect of a die-roll.
     */
    @Override
    protected void doResults(int[] results) {
      for (int i = 0; i < results.length; ++i) {
        final ImageOp icon   = getResultIcon(i, results[i]);
        if (icon !=  null) {
          todraw[i]  = icon;
          widths[i]  = (int)(buttonScale * icon.getWidth());
          heights[i] = (int)(buttonScale * icon.getHeight());
        }
      }
    }
    /**
     * Paint the icon.  This relies on the current set of icons being
     * set in `todraw`.
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
      if (!waiting)
        doPaint(c, g, x, y, true, bgColor);
    }

    /**
     * Get the required width of the icon by investigating the sum
     * width of all the images.
     */
    @Override
    public int getIconWidth() {
      int w = width;
            
      if (todraw != null) {
        int ww = 0;
                
        for (final ImageOp icon : todraw) {
          if (icon == null)
            continue;
          ww += icon.getWidth();
        }
                
        w = Math.max(ww, w);
      }
      return (int)(w * buttonScale);
    }

    /**
     * Get the required height of the icon by investigating the
     * maximum height of all the images.
     */
    @Override
    public int getIconHeight() {
      int h = height;
            
      if (todraw != null) {
        for (final ImageOp icon : todraw) {
          if (icon == null) continue;
          h = Math.max(icon.getHeight(), h);
        }
      }
      return (int)(h * buttonScale);
    }
    /**
     * Set whether we're waiting for animation to finish.  Blocks
     * redrawing.
     */ 
    public void setWaiting(boolean b) {
      waiting = b;
    }
  }        
  /** Icon class for graphical display of a dice roll */
  private class AnimResultsIcon extends BaseResultsIcon {
    // FIXME: because Sun checks what class Icon implementations are,
    // this won't display as disabled properly
    private ImageOp[][] icons;

    int height;
    /**
     * For a given die and a given result, find the indexes of upto
     * `steps` other faces, excluding the face that corresponds to
     * result. If there are not enough faces to fill a `steps` size
     * array, it will be end-padded with the `result` index.  That
     * means, that some die, in particular those that have n-sides
     * equal to or less than `steps` will finish sooner than other
     * dice.
     */
    private List<Integer> getFrameNos(int diceNo, int result)  throws UnsupportedOperationException {
      final int           nfaces   = dice.get(diceNo).getFaceCount();
      final List<Integer> possible = new ArrayList<>(nfaces - 1);
      for (int i = 0; i < nfaces; i++) {
        if (i != result) possible.add(i);
      }
            
      Collections.shuffle(possible, ran);

      final int nsel = Math.min(steps, nfaces - 1);
      final List<Integer> sub = new ArrayList<>(steps);
      for (int i = 0; i < steps; i++) {
        sub.add(i < nsel ? possible.get(i) : result);
      }

      return sub;
    }

    /**
     * Set the results of the die-rolls.
     *
     * This samples the die faces - except the result face - for
     * `steps` images, and builds a table of `steps+1` times N-dice
     * (`steps+1` rows, N-dice columns) icons, which we can then
     * iterate over when animating.  The last row is the result.
     * Note, intermediate images (the first `steps` rows) are randomly
     * rotated to create an effect of a die-roll.
     */
    @Override
    protected void doResults(int[] results) {
      height    = 0;
      icons     = new ImageOp[steps + 1][results.length];
      for (int i = 0; i < results.length; ++i) {
        final List<Integer> frames = getFrameNos(i, results[i]);
        final ImageOp       icon   = getResultIcon(i, results[i]);
        int                 k      = 0;
        for (final int j : frames) {
          final ImageOp curr;
          if (j == results[i])
            curr = icons[k++][i] = icon;
          else 
            curr = icons[k++][i] = rotateImage(getFaceImage(i, j));
          widths[i]   = Math.max(widths[i],  curr.getWidth());
          heights[i]  = Math.max(heights[i], curr.getHeight());
        }
                
        if (icon != null) {
          icons[k][i] = icon;
        }
      }
      todraw = icons[0];
    }

    /**
     * Set the current step in the animation.
     */
    public void setStep(int step) {
      todraw = icons[step];
    }

    /**
     * Paint the icon.  This relies on the current set of icons
     * being set in `todraw`.
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
      doPaint(c, g, x, y, false, null);
    }

    /**
     * Get the required width of the icon by investigating the sum
     * width of all the images.
     */
    @Override
    public int getIconWidth() {
      int w = width;

      if (widths != null) {
        w = 0;
        for (final int ww : widths) w += ww;
        return w;
      }
      
      if (icons != null) {
        for (final ImageOp[] inner : icons) {
          if (inner == null) continue;
                    
          int ww = 0;
                    
          for (final ImageOp icon : inner) {
            if (icon == null) continue;
            ww += icon.getWidth();
          }
                    
          w = Math.max(ww, w);
        }
      }
      return w;
    }

    /**
     * Get the required height of the icon by investigating the
     * maximum height of all the images.
     */
    @Override
    public int getIconHeight() {
      int h = height;
            
      if (icons != null) {
        for (final ImageOp[] inner : icons) {
          if (inner == null) continue;
                    
          for (final ImageOp icon : inner) {
            if (icon == null) continue;
            h = Math.max(icon.getHeight(), h);
          }
        }
      }
      return h;
    }
  }
}


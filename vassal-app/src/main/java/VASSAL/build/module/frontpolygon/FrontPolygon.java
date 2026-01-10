package VASSAL.build.module.frontpolygon;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.Drawable;

import javax.swing.JButton;
import java.awt.Graphics;
import java.awt.Point;
import java.util.EnumMap;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Runtime-only front-line overlay with toolbar toggles.
 */
public class FrontPolygon implements GameComponent, Drawable {
  private static final Set<FrontPolygon> INSTANCES = ConcurrentHashMap.newKeySet();
  private static final ThreadLocal<Boolean> CAPTURE_ACTIVE = ThreadLocal.withInitial(() -> Boolean.FALSE);
  private static volatile boolean renderDuringCaptureOnly = false; // If this is set to true from the outside, then we do not render the polygon twice in each step during logrendering (once of the visible UI and once for the headless paint that goes to ffmpeg)
  private static final AtomicLong HEX_RENDER_NANOS = new AtomicLong();
  private static final AtomicLong SIDE_RENDER_NANOS = new AtomicLong();

  private final Map map;
  private final JButton hexButton;
  private final JButton sideButton;
  private final FrontPolygonCollector collector = new FrontPolygonCollector();
  private final HexFrontRenderer hexRenderer = new HexFrontRenderer();
  private final SideFrontRenderer sideRenderer = new SideFrontRenderer();

  private boolean hexVisible;
  private boolean sideVisible;

  public FrontPolygon(Map map) {
    this.map = map;

    hexButton = new JButton("Hex Front");
    hexButton.setFocusable(false);
    hexButton.addActionListener(e -> toggleHex());

    sideButton = new JButton("Side Front");
    sideButton.setFocusable(false);
    sideButton.addActionListener(e -> toggleSide());

    map.getToolBar().add(hexButton);
    map.getToolBar().add(sideButton);
    map.getToolBar().revalidate();
    map.addDrawComponent(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    INSTANCES.add(this);
    updateButtonText();
  }

  private void toggleHex() {
    final boolean newState = !hexVisible;
    if (newState) {
      // Only one renderer can be active at a time.
      sideVisible = false;
    }
    setHexVisible(newState);
  }

  private void toggleSide() {
    final boolean newState = !sideVisible;
    if (newState) {
      // Only one renderer can be active at a time.
      hexVisible = false;
    }
    setSideVisible(newState);
  }

  private void setHexVisible(boolean show) {
    if (hexVisible == show) {
      return;
    }
    hexVisible = show;
    updateButtonText();
    map.repaint();
  }

  private void setSideVisible(boolean show) {
    if (sideVisible == show) {
      return;
    }
    sideVisible = show;
    updateButtonText();
    map.repaint();
  }

  private void updateButtonText() {
    hexButton.setText(hexVisible ? "Hide Hex Front" : "Show Hex Front");
    sideButton.setText(sideVisible ? "Hide Side Front" : "Show Side Front");
  }

  public void dispose() {
    INSTANCES.remove(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    map.removeDrawComponent(this);
    map.getToolBar().remove(hexButton);
    map.getToolBar().remove(sideButton);
    map.getToolBar().revalidate();
    map.getToolBar().repaint();
  }

  public static Optional<Boolean> toggleAllInstances() {
    if (INSTANCES.isEmpty()) {
      return Optional.empty();
    }

    final boolean anyVisible = INSTANCES.stream().anyMatch(FrontPolygon::isAnyVisible);
    final boolean newState = !anyVisible;
    INSTANCES.forEach(instance -> instance.setBothVisible(newState));
    return Optional.of(newState);
  }

  private boolean isAnyVisible() {
    return hexVisible || sideVisible;
  }

  private void setBothVisible(boolean visible) {
    hexVisible = visible;
    sideVisible = false;
    updateButtonText();
    map.repaint();
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (renderDuringCaptureOnly && !Boolean.TRUE.equals(CAPTURE_ACTIVE.get())) {
      return;
    }
    if (!hexVisible && !sideVisible) {
      return;
    }
    final EnumMap<FrontPolygonSide, List<Point>> sidePoints = collector.collect(this.map);
    if (hexVisible) {
      hexRenderer.render(g, this.map, sidePoints);
    }
    else if (sideVisible) {
      sideRenderer.render(g, this.map, sidePoints);
    }
  }

  public static void setRenderDuringCaptureOnly(boolean enabled) {
    renderDuringCaptureOnly = enabled;
  }

  public static boolean isRenderDuringCaptureOnly() {
    return renderDuringCaptureOnly;
  }

  public static void beginCapture() {
    CAPTURE_ACTIVE.set(Boolean.TRUE);
  }

  public static void endCapture() {
    CAPTURE_ACTIVE.set(Boolean.FALSE);
  }

  public static void addHexRenderNanos(long nanos) {
    HEX_RENDER_NANOS.addAndGet(nanos);
  }

  public static void addSideRenderNanos(long nanos) {
    SIDE_RENDER_NANOS.addAndGet(nanos);
  }

  public static long getHexRenderNanos() {
    return HEX_RENDER_NANOS.get();
  }

  public static long getSideRenderNanos() {
    return SIDE_RENDER_NANOS.get();
  }

  public static void resetRenderNanos() {
    HEX_RENDER_NANOS.set(0);
    SIDE_RENDER_NANOS.set(0);
  }

  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  @Override
  public void setup(boolean gameStarting) { }

  @Override
  public VASSAL.command.Command getRestoreCommand() {
    return null;
  }
}

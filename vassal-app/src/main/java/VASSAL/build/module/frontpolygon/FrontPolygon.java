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

/**
 * Runtime-only front-line overlay with toolbar toggles.
 */
public class FrontPolygon implements GameComponent, Drawable {
  private static final Set<FrontPolygon> INSTANCES = ConcurrentHashMap.newKeySet();

  private final Map map;
  private final JButton hexButton;
  private final JButton sideButton;
  private final FrontPolygonCollector collector = new FrontPolygonCollector();
  private final FrontPolygonRenderer renderer = new FrontPolygonRenderer();

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
    setHexVisible(!hexVisible);
  }

  private void toggleSide() {
    setSideVisible(!sideVisible);
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
    sideVisible = visible;
    updateButtonText();
    map.repaint();
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (!hexVisible && !sideVisible) {
      return;
    }
    final EnumMap<FrontPolygonSide, List<Point>> sidePoints = collector.collect(this.map);
    renderer.render(g, this.map, hexVisible, sideVisible, sidePoints);
  }

  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  @Override
  public void setup(boolean gameStarting) {
    // Reset to hidden whenever a game starts/stops.
    setBothVisible(false);
  }

  @Override
  public VASSAL.command.Command getRestoreCommand() {
    return null;
  }
}

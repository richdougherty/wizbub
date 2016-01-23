package com.richdougherty.wizbub.desktop;

import com.badlogic.gdx.Files;
import com.badlogic.gdx.backends.lwjgl.LwjglApplication;
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration;
import com.richdougherty.wizbub.WizbubGame;

public class DesktopLauncher {
	public static void main (String[] arg) {
		LwjglApplicationConfiguration config = new LwjglApplicationConfiguration();
		config.title = "Wizbub";
		config.addIcon("dawnlike/Derived/icon128.png", Files.FileType.Internal);
		config.addIcon("dawnlike/Derived/icon32.png", Files.FileType.Internal);
		config.addIcon("dawnlike/Derived/icon16.png", Files.FileType.Internal);
		new LwjglApplication(new WizbubGame(), config);
	}
}

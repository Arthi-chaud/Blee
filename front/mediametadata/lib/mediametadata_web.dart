// In order to *not* need this ignore, consider extracting the "web" version
// of your plugin as a separate package, instead of inlining it in the same
// package as the core of your plugin.
// ignore: avoid_web_libraries_in_flutter
import 'dart:html' as html;

import 'package:flutter_web_plugins/flutter_web_plugins.dart';

import 'mediametadata_platform_interface.dart';

/// A web implementation of the MediametadataPlatform of the Mediametadata plugin.
class MediametadataWeb extends MediametadataPlatform {
  /// Constructs a MediametadataWeb
  MediametadataWeb();

  static void registerWith(Registrar registrar) {
    MediametadataPlatform.instance = MediametadataWeb();
  }

  /// Returns a [String] containing the version of the platform.
  @override
  Future<String?> getPlatformVersion() async {
    final version = html.window.navigator.userAgent;
    return version;
  }

  void setMediaMetadata(
      {required String artistName,
      required String? albumName,
      required String songName,
      required String imageUri}) {
    html.window.navigator.mediaSession!.metadata = html.MediaMetadata({
      'title': songName,
      'album': albumName ?? '',
      'artist': artistName,
      'artwork': [
        {
          'src': imageUri,
        },
      ],
    });
  }
}

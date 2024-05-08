import 'package:plugin_platform_interface/plugin_platform_interface.dart';

import 'mediametadata_method_channel.dart';

abstract class MediametadataPlatform extends PlatformInterface {
  /// Constructs a MediametadataPlatform.
  MediametadataPlatform() : super(token: _token);

  static final Object _token = Object();

  static MediametadataPlatform _instance = MethodChannelMediametadata();

  /// The default instance of [MediametadataPlatform] to use.
  ///
  /// Defaults to [MethodChannelMediametadata].
  static MediametadataPlatform get instance => _instance;

  /// Platform-specific implementations should set this with their own
  /// platform-specific class that extends [MediametadataPlatform] when
  /// they register themselves.
  static set instance(MediametadataPlatform instance) {
    PlatformInterface.verifyToken(instance, _token);
    _instance = instance;
  }

  Future<String?> getPlatformVersion() {
    throw UnimplementedError('platformVersion() has not been implemented.');
  }

  void setMediaMetadata(
      {required String artistName,
      required String? albumName,
      required String songName,
      required String imageUri}) {
    throw UnimplementedError('setMediaMetadata() has not been implemented.');
  }
}

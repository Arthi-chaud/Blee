import 'package:flutter/foundation.dart';
import 'package:flutter/services.dart';

import 'mediametadata_platform_interface.dart';

/// An implementation of [MediametadataPlatform] that uses method channels.
class MethodChannelMediametadata extends MediametadataPlatform {
  /// The method channel used to interact with the native platform.
  @visibleForTesting
  final methodChannel = const MethodChannel('mediametadata');

  @override
  Future<String?> getPlatformVersion() async {
    final version = await methodChannel.invokeMethod<String>('getPlatformVersion');
    return version;
  }
}

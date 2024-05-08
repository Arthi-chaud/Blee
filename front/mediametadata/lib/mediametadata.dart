import 'mediametadata_platform_interface.dart';

class Mediametadata {
  Future<String?> getPlatformVersion() {
    return MediametadataPlatform.instance.getPlatformVersion();
  }

  void setMediaMetadata(
      {required String artistName,
      required String? albumName,
      required String songName,
      required String imageUri}) {
    MediametadataPlatform.instance.setMediaMetadata(
        artistName: artistName,
        albumName: albumName,
        songName: songName,
        imageUri: imageUri);
  }
}

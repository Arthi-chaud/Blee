import 'package:blee/api/api.dart' as api;

enum PlayerFlowStep {
  // Loading needed metadata from API
  loadingNeededMetadata,
  // The video controller is loading
  loadingPlayer,
  // playback started
  playerStarted,
  // an error occured while loading resources
  errored
}

class PlayerMetadata {
  final String videoTitle;
  final String videoArtist;
  // The url to the parent Package
  final String parentUrlResource;
  final api.Image? poster;
  final api.Image? thumbnail;
  final api.File videoFile;

  PlayerMetadata(
      {required this.videoTitle,
      required this.parentUrlResource,
      required this.videoArtist,
      required this.poster,
      required this.thumbnail,
      required this.videoFile});
}
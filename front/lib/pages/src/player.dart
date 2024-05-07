import 'package:blee/models/models.dart';
import 'package:blee/providers.dart';
import 'package:blee/ui/src/image.dart';
import 'package:blee/ui/src/player_controls.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

class PlayerPage extends ConsumerStatefulWidget {
  final String? extraUuid;
  final String? movieUuid;
  const PlayerPage({super.key, this.movieUuid, this.extraUuid});

  @override
  PlayerPageState createState() => PlayerPageState();
}

class PlayerPageState extends ConsumerState<PlayerPage> {
  PlayerFlowStep flowStep = PlayerFlowStep.loadingNeededMetadata;
  late AutoDisposeFutureProvider<PlayerMetadata> videoMetadataProvider;

  @override
  void initState() {
    super.initState();
    videoMetadataProvider = widget.extraUuid != null
        ? getPlayerMetadataFromExtraUuidProvider(widget.extraUuid!)
        : getPlayerMetadataFromMovieUuidProvider(widget.movieUuid!);
  }

  @override
  Widget build(BuildContext context) {
    final metadata = ref.watch(videoMetadataProvider)
      ..when(
          data: (_) {
            setState(() {
              flowStep = PlayerFlowStep.loadingPlayer;
            });
          },
          error: (_, __) {
            setState(() {
              flowStep = PlayerFlowStep.errored;
            });
          },
          loading: () {});
    return Scaffold(
        backgroundColor: Colors.black,
        body: Stack(
          children: [
            Thumbnail(
                image: metadata.value?.thumbnail,
                disableSlashFadein: true,
                disableBorderRadius: true),
            Container(color: Colors.black.withAlpha(200)),
            const Center(
              child: CircularProgressIndicator(),
            ),
            PlayerControls(
              title: metadata.value?.videoTitle,
              poster: metadata.value?.poster,
              subtitle: metadata.value?.videoArtist,
              duration: metadata.value?.videoFile.duration,
              progress: null,
            )
          ],
        ));
  }
}

import 'package:blee/models/models.dart';
import 'package:blee/providers.dart';
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
  PlayerPageState() {
    videoMetadataProvider = widget.extraUuid != null
        ? getPlayerMetadataFromExtraUuidProvider(widget.extraUuid!)
        : getPlayerMetadataFromMovieUuidProvider(widget.movieUuid!);
  }

  @override
  void initState() {
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    final metadata = ref.watch(videoMetadataProvider).when(
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
    // .when(data: data, error: error, loading: loading);
    return Container(
      child: Text(flowStep.name),
    );
  }
}

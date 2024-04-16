import 'package:blee/api/src/models/image.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'chapter.freezed.dart';
part 'chapter.g.dart';

@freezed
class Chapter with _$Chapter {
  const factory Chapter({
    required String id,
    required String name,
    @JsonKey(name: "movie_id")
    required String movieId,
    @JsonKey(name: "start_time")
    required int startTime,
    @JsonKey(name: "end_time")
    required int endTime,
    required Image? thumbnail,
    required List<ChapterType> type,
  }) = _Chapter;

  factory Chapter.fromJson(Map<String, dynamic> json) => _$ChapterFromJson(json);
}

enum ChapterType {
  @JsonValue("interview")
  Interview,
  @JsonValue("non_musical_interview")
  NonMusicalInterview,
  @JsonValue("other")
  Other,
  @JsonValue("performance")
  Performance
}
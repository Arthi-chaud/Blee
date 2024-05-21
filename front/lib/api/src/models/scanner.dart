import 'package:freezed_annotation/freezed_annotation.dart';

part 'scanner.freezed.dart';
part 'scanner.g.dart';

@freezed
class ScannerStatusResponse with _$ScannerStatusResponse {
  const factory ScannerStatusResponse({
    required ScannerStatus status,
  }) = _ScannerStatusResponse;

  factory ScannerStatusResponse.fromJson(Map<String, dynamic> json) =>
      _$ScannerStatusResponseFromJson(json);
}

enum ScannerStatus {
  @JsonValue("idle")
  idle,
  @JsonValue("scan")
  scan,
  @JsonValue("clean")
  clean,
}

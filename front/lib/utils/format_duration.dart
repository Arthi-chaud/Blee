/// 125 => 2:05
///
String formatDuration(int durationInSeconds) {
  var seconds = durationInSeconds % 60;
  var minutes = durationInSeconds ~/ 60 % 60;
  var hours = durationInSeconds ~/ 3600;
  var formatedUpToMinutes =
      "${minutes.toString().padLeft(2, '0')}:${seconds.toString().padLeft(2, '0')}";
  if (hours > 0) {
    return "$hours:$formatedUpToMinutes";
  }
  return formatedUpToMinutes;
}

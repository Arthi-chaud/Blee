import 'package:blee/api/api.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';
part 'providers.g.dart';

@riverpod
Future<Package> getPackage(GetPackageRef ref, String packageUuid) async {
  return await APIClient().getPackage(packageUuid);
}

@riverpod
Future<Page<Movie>> getMovies(GetMoviesRef ref, {String? packageUuid}) async {
  return await APIClient().getMovies(packageUuid!);
}

@riverpod
Future<List<Chapter>> getChapters(GetChaptersRef ref, String movieUuid) async {
  return await APIClient().getChapters(movieUuid);
}

@riverpod
Future<Page<Extra>> getExtras(GetExtrasRef ref,
    {String? packageUuid, PageQuery page = const PageQuery()}) async {
  return await APIClient().getExtras(packageUuid!, page);
}

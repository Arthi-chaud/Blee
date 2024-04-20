import 'package:blee/api/api.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';
import 'package:skeletonizer/skeletonizer.dart';

part 'main.g.dart';

@riverpod
Future<Artist> activity(ActivityRef ref) async {
  return await APIClient().getArtist("the-corrs");
}

void main() {
  runApp(const ProviderScope(
    child: MyApp(),
  ));
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter Demo',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple),
        useMaterial3: true,
      ),
      home: const MyHomePage(),
    );
  }
}

class MyHomePage extends StatelessWidget {
  const MyHomePage({super.key});

  @override
  Widget build(BuildContext context) {
    return Consumer(
      builder: (context, ref, child) {
        final AsyncValue<Artist> data = ref.watch(activityProvider);

        return Center(
            child: Skeletonizer(
                enabled: data.isLoading,
                child: switch (data) {
                  AsyncData(:final value) => Text(value.name),
                  AsyncError(:final error) => Text(error.toString()),
                  _ => Container(),
                }));
      },
    );
  }
}

import 'package:blee/api/src/models/scanner.dart';
import 'package:blee/providers.dart';
import 'package:change_case/change_case.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:skeletonizer/skeletonizer.dart';

class SettingsPage extends ConsumerWidget {
  const SettingsPage({super.key});

  String stringFromScannerStatus(ScannerStatus status) {
    return status.name.toTitleCase();
  }

  @override
  Widget build(BuildContext context, ref) {
    var scannerStatus = ref.watch(getScannerStatusProvider);
    var apiClient = ref.read(apiClientProvider);
    return Padding(
      padding: const EdgeInsets.symmetric(horizontal: 16),
      child: ListView(
        children: [
          Padding(
            padding: const EdgeInsets.symmetric(vertical: 8.0),
            child: Text(
              "Scanner",
              style: Theme.of(context).textTheme.titleLarge,
            ),
          ),
          Column(
            children: [
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  const Text("Status"),
                  Skeletonizer(
                      enabled: scannerStatus.value == null,
                      child: Text(scannerStatus.value != null
                          ? stringFromScannerStatus(scannerStatus.value!.status)
                          : "No Status"))
                ],
              ),
              SizedBox.fromSize(
                size: const Size.fromHeight(8),
              ),
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: [
                  Flexible(
                      child: ElevatedButton.icon(
                          label: const Text("Scan new files"),
                          onPressed: () {
                            apiClient.requestScan().whenComplete(
                                () => ref.invalidate(getScannerStatusProvider));
                          },
                          icon: const FaIcon(FontAwesomeIcons.rotate))),
                  ElevatedButton.icon(
                      label: const Text("Clean Library"),
                      onPressed: () {
                        apiClient.requestClean().whenComplete(
                            () => ref.invalidate(getScannerStatusProvider));
                      },
                      icon: const FaIcon(FontAwesomeIcons.broom)),
                ],
              )
            ],
          ),
        ],
      ),
    );
  }
}

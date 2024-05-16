import 'package:flutter/material.dart';
import 'package:skeletonizer/skeletonizer.dart';

class DescriptionBox extends StatelessWidget {
  final String? description;
  final bool skeletonize;

  const DescriptionBox(
      {super.key, required this.description, required this.skeletonize});

  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        Padding(
          padding: const EdgeInsets.symmetric(horizontal: 8.0, vertical: 4),
          child: skeletonize
              ? const Skeletonizer.zone(child: Bone.multiText(lines: 3))
              : Text(
                  description ?? '',
                  maxLines: 3,
                  overflow: TextOverflow.ellipsis,
                ),
        ),
        Positioned.fill(
            child: ClipRRect(
                borderRadius: BorderRadius.circular(8),
                child: Material(
                    color: Colors.transparent,
                    child: description == null
                        ? Container()
                        : InkWell(
                            onTap: () => _showFullTextDialog(context),
                          )))),
      ],
    );
  }

  Future<void> _showFullTextDialog(BuildContext context) {
    return showDialog(
        context: context,
        builder: (context) {
          return AlertDialog(
            title: const Text('Description'),
            content: Text(description!),
            actions: [
              TextButton(
                  onPressed: () => Navigator.of(context).pop(),
                  child: const Text('Close'))
            ],
          );
        });
  }
}

import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';

class StarRating extends StatelessWidget {
  // Rating should be betweem 0 and 100
  final int rating;
  final double starSize;
  const StarRating({super.key, required this.rating, this.starSize = 10});

  int _normalizeStarIndex(int index) {
    return (index - 1) * 20;
  }

  FaIcon _buildIcon(IconData starIcon, BuildContext context) {
    return FaIcon(starIcon,
        size: starSize,
        color: Theme.of(context).colorScheme.onPrimaryContainer);
  }

  @override
  Widget build(BuildContext context) {
    return Tooltip(
        message: "$rating / 100",
        child: Padding(
            padding: const EdgeInsets.only(bottom: 1),
            child: Row(
              mainAxisSize: MainAxisSize.min,
              children: List.generate(5, (i) => i + 1, growable: false)
                  .map((starIndex) {
                final starThreshold = _normalizeStarIndex(starIndex);
                if (rating <= starThreshold) {
                  return _buildIcon(FontAwesomeIcons.star, context);
                } else if (starThreshold <= rating &&
                    rating < _normalizeStarIndex(starIndex + 1)) {
                  return _buildIcon(
                      FontAwesomeIcons.solidStarHalfStroke, context);
                }
                return _buildIcon(FontAwesomeIcons.solidStar, context);
              }).toList(),
            )));
  }
}

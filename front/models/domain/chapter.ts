import * as yup from "yup";
import { Resource } from "./resource";
import { Image } from "./image";

const ChapterTypes = [
    "interview",
    "non_musical_interview",
    "other",
    "performance",
];

type ChapterType = (typeof ChapterTypes)[number];

const Chapter = Resource.concat(
    yup.object({
        name: yup.string().required(),
        movie_id: yup.string().required(),
        start_time: yup.number().required(),
        end_time: yup.number().required(),
        thumbnail: Image.required().nullable(),
        type: yup
            .array(yup.mixed<ChapterType>().oneOf(ChapterTypes).required())
            .required(),
    }),
);

type Chapter = yup.InferType<typeof Chapter>;

export { type ChapterType, Chapter };

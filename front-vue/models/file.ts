import * as yup from "yup";
import { Resource } from "./resource";
import { type VideoQuality, VideoQualities } from "./video-quality";

const File = Resource.concat(
    yup.object({
        duration: yup.number().required(),
        quality: yup.mixed<VideoQuality>().oneOf(VideoQualities).required(),
    }),
);

type File = yup.InferType<typeof File>;

export { File };

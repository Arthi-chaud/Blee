import * as yup from "yup";

export const FileInfo = yup.object({
    mimeCodec: yup.string().required().nullable(),
    videos: yup
        .array(yup.object({ mimeCodec: yup.string().required().nullable() }))
        .required(),
    audios: yup
        .array(yup.object({ codec: yup.string().required().nullable() }))
        .required(),
});

export type FileInfo = yup.InferType<typeof FileInfo>;

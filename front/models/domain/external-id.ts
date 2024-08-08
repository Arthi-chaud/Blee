import * as yup from "yup";

const ExternalId = yup.object({
    url: yup.string().required(),
    value: yup.string().required(),
    description: yup.string().required().nullable(),
    rating: yup.number().required().nullable(),
    provider_name: yup.string().required(),
});

type ExternalId = yup.InferType<typeof ExternalId>;

export { ExternalId };

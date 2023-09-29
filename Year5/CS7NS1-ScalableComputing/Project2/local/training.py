#!/usr/bin/env python3

import sys
import os
import math
import base64
import multiprocessing
import argparse
import random
import datetime
import statistics
import numpy as np
import cv2 as cv
from captcha.image import ImageCaptcha
from PIL import ImageFilter

CHARSET = "#%+-:[\]{}0123456789ABDFMPQRTUVWXYZceghjkns"
WIDTH = 128; HEIGHT = 64
CH_MIN = 1; CH_MAX = 6
BG_MIN = 238; BG_MAX = 255
FG_MIN = 10; FG_MAX = 200
OP_MIN = 220; OP_MAX = 255
VALIDATION_RATIO = 0.1
BATCH_SIZE = 128


def generate_captcha_pairs(noisy_dir_path, clean_dir_path, font_file_path, font_size, count):
    time = datetime.datetime.now().strftime("%y%m%d%H%M%s")
    captcha = ImageCaptcha(width=WIDTH, height=HEIGHT, fonts=[font_file_path], font_sizes=(font_size,))
    if not os.path.isdir(noisy_dir_path): os.makedirs(noisy_dir_path)
    if not os.path.isdir(clean_dir_path): os.makedirs(clean_dir_path)
    print(f"Generating {count} new captchas image pairs...")
    args_list = [ (i, time, captcha, noisy_dir_path, clean_dir_path) for i in range(count) ]
    multiprocessing.Pool().map(_generate_captcha_pair, args_list)


def _generate_captcha_pair(args):
    i, time, captcha, noisy_dir_path, clean_dir_path = args
    chars = random.sample(CHARSET, random.randint(CH_MIN, CH_MAX))
    bg_colour = (random.randint(BG_MIN, BG_MAX), random.randint(BG_MIN, BG_MAX), random.randint(BG_MIN, BG_MAX))
    fg_colour = (random.randint(FG_MIN, FG_MAX), random.randint(FG_MIN, FG_MAX), random.randint(FG_MIN, FG_MAX), random.randint(OP_MIN, OP_MAX))
    clean_image = captcha.create_captcha_image(chars, fg_colour, bg_colour)
    noisy_image = clean_image.copy()
    captcha.create_noise_dots(noisy_image, fg_colour)
    captcha.create_noise_curve(noisy_image, fg_colour)
    noisy_image = noisy_image.filter(ImageFilter.SMOOTH)
    clean_image = clean_image.filter(ImageFilter.SMOOTH)
    b64chars = base64.urlsafe_b64encode("".join(chars).encode("ascii")).decode()
    noisy_image.save(os.path.join(noisy_dir_path, f"{time}-{i}.{b64chars}.png"))
    clean_image.save(os.path.join(clean_dir_path, f"{time}-{i}.{b64chars}.png"))


def preprocess_images(src_dir_path, dst_dir_path, max_count):
    file_names = sorted(os.listdir(src_dir_path))[:max_count]
    file_path_pairs = [ (os.path.join(src_dir_path, file_name), os.path.join(dst_dir_path, file_name)) for file_name in file_names ]
    if not os.path.isdir(dst_dir_path): os.makedirs(dst_dir_path)
    print(f"Preprocessing {len(file_names)} images...")
    multiprocessing.Pool().map(_preprocess_image, file_path_pairs)


def _preprocess_image(file_path_pair):
    src_file_path, dst_file_path = file_path_pair
    image = cv.imread(src_file_path)
    image = np.subtract(255, image)
    image = cv.cvtColor(image, cv.COLOR_RGB2GRAY)
    bg_color = statistics.mode(image[[0,0,-1,-1],[0,-1,0,-1]])
    image = np.subtract(image.astype(np.int16), np.uint8(bg_color))
    image = image.clip(0, 255).astype(np.uint8)
    image = cv.normalize(image, None, 0, 255, cv.NORM_MINMAX)
    image = cv.flip(image, 0)
    image = cv.transpose(image)
    cv.imwrite(dst_file_path, image)


def clean_images(src_dir_path, dst_dir_path, max_count, cleaner_path):
    file_names = sorted(os.listdir(src_dir_path))[:max_count]
    src_file_paths = [ os.path.join(src_dir_path, file_name) for file_name in file_names ]
    dst_file_paths = [ os.path.join(dst_dir_path, file_name) for file_name in file_names ]
    if not os.path.isdir(dst_dir_path): os.makedirs(dst_dir_path)
    print(f"Cleaning {len(file_names)} images...")
    import tensorflow as tf
    with tf.device("/CPU:0"): # TODO(ted): remove me
        cleaner = tf.keras.models.load_model(cleaner_path, compile=False)
        def input_pipeline(file_path):
            image = tf.io.read_file(file_path)
            image = tf.io.decode_png(image, channels=1)
            return tf.image.convert_image_dtype(image, tf.float32)
        dataset = tf.data.Dataset.from_tensor_slices(src_file_paths) \
            .map(input_pipeline, num_parallel_calls=tf.data.AUTOTUNE) \
            .batch(BATCH_SIZE).prefetch(buffer_size=tf.data.AUTOTUNE)
        for i, images in enumerate(dataset):
            for j, image in enumerate(cleaner(images, training=False)):
                tf.keras.utils.save_img(dst_file_paths[i*BATCH_SIZE+j], image)


def decode_images(src_dir_path, max_count, decoder_path):
    file_names = sorted(os.listdir(src_dir_path))[:max_count]
    file_paths = [ os.path.join(src_dir_path, file_name) for file_name in file_names ]
    print(f"Decoding {len(file_names)} images...")
    import tensorflow as tf
    with tf.device("/CPU:0"): # TODO(ted): remove me
        #decoder = tf.keras.models.load_model(decoder_path, compile=False)

        ## TODO(ted): remove me
        @tf.keras.saving.register_keras_serializable()
        class CTCLayer(tf.keras.layers.Layer):
            def __init__(self, name=None):
                super().__init__(name=name)
                self.loss_fn = tf.keras.backend.ctc_batch_cost
            def call(self, chars_true, chars_pred):
                batch_len = tf.cast(tf.shape(chars_true)[0], dtype="int64")
                input_len = tf.cast(tf.shape(chars_pred)[1], dtype="int64") * tf.ones(shape=(batch_len, 1), dtype="int64")
                label_len = tf.cast(tf.shape(chars_true)[1], dtype="int64") * tf.ones(shape=(batch_len, 1), dtype="int64")
                self.add_loss(self.loss_fn(chars_true, chars_pred, input_len, label_len))
                return chars_pred
        decoder = tf.keras.models.load_model(decoder_path, compile=False)
        decoder = tf.keras.models.Model(decoder.get_layer(name="image").input, decoder.get_layer(name="last_layer").output)

        num_to_char = tf.keras.layers.StringLookup(vocabulary=list(CHARSET), invert=True)
        def input_pipeline(file_path):
            image = tf.io.read_file(file_path)
            image = tf.io.decode_png(image, channels=1)
            return tf.image.convert_image_dtype(image, tf.float32)
        dataset = tf.data.Dataset.from_tensor_slices(file_paths) \
            .map(input_pipeline, num_parallel_calls=tf.data.AUTOTUNE) \
            .batch(BATCH_SIZE).prefetch(buffer_size=tf.data.AUTOTUNE)
        for i, images in enumerate(dataset):
            results = decoder(images, training=False)
            results_shape = np.ones(results.shape[0]) * results.shape[1]
            results = tf.keras.backend.ctc_decode(results, input_length=results_shape, greedy=False, top_paths=5)
            for j, probs in enumerate(results[1]):
                print(file_names[i*BATCH_SIZE+j])
                for k, prob in enumerate(probs):
                    prob = int(math.e ** prob.numpy() * 100)
                    path = results[0][k][j][:CH_MAX]
                    path = tf.gather(path, tf.where(tf.math.not_equal(path, -1)))
                    path = tf.strings.reduce_join(num_to_char(path))
                    path = path.numpy().decode("utf-8")
                    print(f"{path} {prob}%")


def train_cleaner(noisy_dir_path, clean_dir_path, epochs, max_count, model_path):
    import tensorflow as tf

    # Ensure model directory exists for saving to
    model_dir_path = os.path.dirname(model_path) or "."
    if not os.path.isdir(model_dir_path):
        os.makedirs(os.path.dirname(model_dir_path))

    # Load or build a model
    if os.path.isfile(model_path):
        model = tf.keras.models.load_model(model_path, compile=True)
        print(f"Loaded cleaner model from {model_path}")
    else:
        inputs = tf.keras.Input((WIDTH, HEIGHT, 1), name="image")
        x = tf.keras.layers.Conv2D(32, (3, 3), activation="relu", padding="same")(inputs)
        x = tf.keras.layers.MaxPooling2D((2, 2), padding="same")(x)
        x = tf.keras.layers.Conv2D(32, (3, 3), activation="relu", padding="same")(x)
        x = tf.keras.layers.MaxPooling2D((2, 2), padding="same")(x)
        x = tf.keras.layers.Conv2DTranspose(32, (3, 3), strides=2, activation="relu", padding="same")(x)
        x = tf.keras.layers.Conv2DTranspose(32, (3, 3), strides=2, activation="relu", padding="same")(x)
        x = tf.keras.layers.Conv2D(1, (3, 3), activation="sigmoid", padding="same", name="last_layer")(x)
        model = tf.keras.models.Model(inputs, x)
        model.compile(optimizer="adam", loss="binary_crossentropy")
        print(f"Creating a new cleaner model at {model_path}")

    # Split training data between training and validation without overlap
    file_names = sorted(os.listdir(noisy_dir_path))[:max_count]
    assert set(file_names) == set(sorted(os.listdir(clean_dir_path))[:max_count]), "Training data directories are inconsistent"
    file_path_pairs = [ (os.path.join(noisy_dir_path, file_name), os.path.join(clean_dir_path, file_name)) for file_name in file_names ]
    file_path_pairs_train = file_path_pairs[int(len(file_names) * VALIDATION_RATIO):]
    file_path_pairs_valid = file_path_pairs[:int(len(file_names) * VALIDATION_RATIO)]
    assert len(file_path_pairs_train) != 0 and len(file_path_pairs_valid) != 0

    # Setup training and validation datasets with batching
    def input_pipeline(file_path_pair):
        noisy_image = tf.io.read_file(file_path_pair[0])
        noisy_image = tf.io.decode_png(noisy_image, channels=1)
        noisy_image = tf.image.convert_image_dtype(noisy_image, tf.float32)
        clean_image = tf.io.read_file(file_path_pair[1])
        clean_image = tf.io.decode_png(clean_image, channels=1)
        clean_image = tf.image.convert_image_dtype(clean_image, tf.float32)
        return noisy_image, clean_image
    training_dataset = tf.data.Dataset.from_tensor_slices(file_path_pairs_train) \
        .map(input_pipeline, num_parallel_calls=tf.data.AUTOTUNE) \
        .batch(BATCH_SIZE).prefetch(buffer_size=tf.data.AUTOTUNE)
    validation_dataset = tf.data.Dataset.from_tensor_slices(file_path_pairs_valid) \
        .map(input_pipeline, num_parallel_calls=tf.data.AUTOTUNE) \
        .batch(BATCH_SIZE).prefetch(buffer_size=tf.data.AUTOTUNE)

    # Train model
    callbacks = [
        tf.keras.callbacks.EarlyStopping(monitor="val_loss", patience=10, restore_best_weights=True),
        tf.keras.callbacks.ModelCheckpoint(model_path)
    ]
    try:
        model.fit(x=training_dataset, validation_data=(validation_dataset,), epochs=epochs, callbacks=callbacks)
        model.save(model_path)
    except KeyboardInterrupt:
        model.save(model_path + ".interrupted.keras")


def train_decoder(src_dir_path, epochs, max_count, training_model_path, inference_model_path):
    import tensorflow as tf

    # Ensure model directories exists for saving to
    training_model_dir_path = os.path.dirname(training_model_path) or "."
    if not os.path.isdir(training_model_dir_path):
        os.makedirs(os.path.dirname(training_model_dir_path))
    inference_model_dir_path = os.path.dirname(inference_model_path) or "."
    if not os.path.isdir(inference_model_dir_path):
        os.makedirs(os.path.dirname(inference_model_dir_path))

    # A custom Keras layer class for CTC loss
    @tf.keras.saving.register_keras_serializable()
    class CTCLayer(tf.keras.layers.Layer):
        def __init__(self, name=None):
            super().__init__(name=name)
            self.loss_fn = tf.keras.backend.ctc_batch_cost
        def call(self, chars_true, chars_pred, training=False):
            batch_len = tf.cast(tf.shape(chars_true)[0], dtype="int64")
            input_len = tf.cast(tf.shape(chars_pred)[1], dtype="int64") * tf.ones(shape=(batch_len, 1), dtype="int64")
            label_len = tf.cast(tf.shape(chars_true)[1], dtype="int64") * tf.ones(shape=(batch_len, 1), dtype="int64")
            self.add_loss(self.loss_fn(chars_true, chars_pred, input_len, label_len))
            return chars_pred

    # Map between charset and integers
    char_to_num = tf.keras.layers.StringLookup(vocabulary=list(CHARSET))
    num_to_char = tf.keras.layers.StringLookup(vocabulary=list(CHARSET), invert=True)

    # Load or build a model
    if os.path.isfile(training_model_path):
        model = tf.keras.models.load_model(training_model_path, compile=True)
        print(f"Loaded decoder model from {training_model_path}")
    else:
        image_input = tf.keras.Input(shape=(WIDTH, HEIGHT, 1), name="image")
        chars_input = tf.keras.layers.Input(shape=(None,), name="chars")
        x = tf.keras.layers.Conv2D(32, (3, 3), activation="relu", kernel_initializer="he_normal", padding="same")(image_input)
        x = tf.keras.layers.MaxPooling2D((2, 2))(x)
        x = tf.keras.layers.Conv2D(64, (3, 3), activation="relu", kernel_initializer="he_normal", padding="same")(x)
        x = tf.keras.layers.MaxPooling2D((2, 2))(x)
        x = tf.keras.layers.Reshape(target_shape=((WIDTH // 4), (HEIGHT // 4) * 64))(x)
        x = tf.keras.layers.Dense(64, activation="relu")(x)
        x = tf.keras.layers.Dropout(0.2)(x)
        x = tf.keras.layers.Bidirectional(tf.keras.layers.LSTM(128, return_sequences=True, dropout=0.25))(x)
        x = tf.keras.layers.Bidirectional(tf.keras.layers.LSTM(64, return_sequences=True, dropout=0.25))(x)
        x = tf.keras.layers.Dense(len(CHARSET) + 2, activation="softmax", name="last_layer")(x)
        outputs = CTCLayer()(chars_input, x)
        model = tf.keras.Model(inputs=[image_input, chars_input], outputs=outputs)
        model.compile(optimizer="adam")
        print(f"Creating a new decoder model at {training_model_path}")

    # Split training data between training and validation without overlap
    file_names = sorted(os.listdir(src_dir_path))[:max_count]
    file_paths = [ os.path.join(src_dir_path, file_name) for file_name in file_names ]
    file_paths_train = file_paths[int(len(file_names) * VALIDATION_RATIO):]
    file_paths_valid = file_paths[:int(len(file_names) * VALIDATION_RATIO)]
    assert len(file_paths_train) != 0 and len(file_paths_valid) != 0

    # Setup training and validation datasets with batching
    def input_pipeline(file_path):
        image = tf.io.read_file(file_path)
        image = tf.io.decode_png(image, channels=1)
        image = tf.image.convert_image_dtype(image, tf.float32)
        chars = tf.io.decode_base64(tf.strings.split(file_path, ".")[-2])
        chars = char_to_num(tf.strings.bytes_split(chars))
        chars = tf.pad(chars, paddings=[[0, CH_MAX - tf.shape(chars)[0]]], constant_values=len(CHARSET)+1)
        return { "image": image, "chars": chars }
    training_dataset = tf.data.Dataset.from_tensor_slices(file_paths_train) \
        .map(input_pipeline, num_parallel_calls=tf.data.AUTOTUNE) \
        .batch(BATCH_SIZE).prefetch(buffer_size=tf.data.AUTOTUNE)
    validation_dataset = tf.data.Dataset.from_tensor_slices(file_paths_valid) \
        .map(input_pipeline, num_parallel_calls=tf.data.AUTOTUNE) \
        .batch(BATCH_SIZE).prefetch(buffer_size=tf.data.AUTOTUNE)

    # Train model
    callbacks = [
        tf.keras.callbacks.EarlyStopping(monitor="val_loss", patience=10, restore_best_weights=True),
        tf.keras.callbacks.ModelCheckpoint(training_model_path)
    ]
    try:
        model.fit(x=training_dataset, validation_data=(validation_dataset,), epochs=epochs, callbacks=callbacks)
        model.save(training_model_path)
        model = tf.keras.models.Model(model.get_layer(name="image").input, model.get_layer(name="last_layer").output)
        model.save(inference_model_path)
    except KeyboardInterrupt:
        model.save(training_model_path + ".interrupted.keras")


def convert_model(src_path, dst_path):
    import tensorflow as tf
    dst_dir_path = os.path.dirname(dst_path) or "."
    if not os.path.isdir(dst_dir_path):
        os.makedirs(os.path.dirname(dst_dir_path))
    model = tf.keras.models.load_model(src_path, compile=True)
    tflite = tf.lite.TFLiteConverter.from_keras_model(model).convert()
    with open(dst_path, "wb") as f:
      f.write(tflite)


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser_subcmd = parser.add_subparsers(dest="subcommand", required=True)

    parser_generate = parser_subcmd.add_parser("generate")
    parser_generate.add_argument("noisy_destination")
    parser_generate.add_argument("clean_destination")
    parser_generate.add_argument("fontfile")
    parser_generate.add_argument("fontsize", type=int)
    parser_generate.add_argument("count", type=int)

    parser_preprocess = parser_subcmd.add_parser("preprocess")
    parser_preprocess.add_argument("source")
    parser_preprocess.add_argument("destination")
    parser_preprocess.add_argument("--count", type=int, default=99999999)

    parser_clean = parser_subcmd.add_parser("clean")
    parser_clean.add_argument("source")
    parser_clean.add_argument("destination")
    parser_clean.add_argument("cleaner")
    parser_clean.add_argument("--count", type=int, default=99999999)

    parser_decode = parser_subcmd.add_parser("decode")
    parser_decode.add_argument("source")
    parser_decode.add_argument("decoder")
    parser_decode.add_argument("--count", type=int, default=99999999)

    parser_train = parser_subcmd.add_parser("train")
    parser_train_model = parser_train.add_subparsers(dest="model", required=True)

    parser_train_cleaner = parser_train_model.add_parser("cleaner")
    parser_train_cleaner.add_argument("noisy_source")
    parser_train_cleaner.add_argument("clean_source")
    parser_train_cleaner.add_argument("epochs", type=int)
    parser_train_cleaner.add_argument("cleaner")
    parser_train_cleaner.add_argument("--count", type=int, default=99999999)

    parser_train_decoder = parser_train_model.add_parser("decoder")
    parser_train_decoder.add_argument("source")
    parser_train_decoder.add_argument("epochs", type=int)
    parser_train_decoder.add_argument("training_decoder")
    parser_train_decoder.add_argument("inference_decoder")
    parser_train_decoder.add_argument("--count", type=int, default=99999999)

    parser_convert = parser_subcmd.add_parser("convert")
    parser_convert.add_argument("source")
    parser_convert.add_argument("destination")

    args = parser.parse_args()

    if args.subcommand == "generate": generate_captcha_pairs(args.noisy_destination, args.clean_destination, args.fontfile, args.fontsize, args.count)
    elif args.subcommand == "preprocess": preprocess_images(args.source, args.destination, args.count)
    elif args.subcommand == "clean": clean_images(args.source, args.destination, args.count, args.cleaner)
    elif args.subcommand == "decode": decode_images(args.source, args.count, args.decoder)
    elif args.subcommand == "train" and args.model == "cleaner": train_cleaner(args.noisy_source, args.clean_source, args.epochs, args.count, args.cleaner)
    elif args.subcommand == "train" and args.model == "decoder": train_decoder(args.source, args.epochs, args.count, args.training_decoder, args.inference_decoder)
    elif args.subcommand == "convert": convert_model(args.source, args.destination)

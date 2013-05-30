#include <stdlib.h>
#include <stdio.h>

#include <pthread.h>
#include <vlc/vlc.h>
#include <string.h>

#define DEFAULT_CHUNK_SIZE 2048

struct vlc_context
{
    libvlc_instance_t *mVlcInstance;
    libvlc_media_player_t *mMp;
    libvlc_media_t *mMedia;

    int mPlaying;
    int16_t* mBuffer;

    int mChunkSize; 

    char* mAudioData;
    unsigned int mAudioDataSize;
    unsigned int mBufferSize;
    unsigned int mFrequency;
    unsigned int mFramesOverlap;

    int mChannels;

    pthread_mutex_t* mLock;
};
typedef struct vlc_context vlc_context;

void prepareRender( void* p_audio_data, uint8_t** pp_pcm_buffer , unsigned int size );


void handleStream(void* p_audio_data, uint8_t* p_pcm_buffer, unsigned int channels, unsigned int rate,
                  unsigned int nb_samples, unsigned int bits_per_sample, unsigned int size, int64_t pts );

void flushBuffer(vlc_context* ctx);
void useBuffer(vlc_context* ctx);


vlc_context*
caml_init_context(char *uri, int chunkSize)
{
    vlc_context* ctx = malloc(sizeof(vlc_context));
    ctx->mPlaying = 0;
    
    char smemOptions[256];

    char* url = uri;

     sprintf(smemOptions, "#transcode{acodec=s16l}:duplicate{dst=display,dst=smem"
         "{audio-postrender-callback=%lld,audio-prerender-callback=%lld,audio-data=%lld}}",
                           (long long int)(intptr_t)(void*)&handleStream,
                           (long long int)(intptr_t)(void*)&prepareRender, 
                           (long long int)(intptr_t)(void*)ctx);

    const char stringSout[] = "--sout", stringNoSoutSmemTimeSync[] = "--no-sout-smem-time-sync";
    const char * const vlcArgs[] = {
        stringSout,
        smemOptions,
        stringNoSoutSmemTimeSync };

    ctx->mVlcInstance = libvlc_new(2, vlcArgs); // deleted in the destructor

    ctx->mMp = libvlc_media_player_new(ctx->mVlcInstance); // deleted in the destructor
    libvlc_audio_set_volume (ctx->mMp,0);

    ctx->mMedia = libvlc_media_new_path (ctx->mVlcInstance, url);
    ctx->mPlaying = 1;
    ctx->mBufferSize = 0;
    ctx->mChannels = 1;
    ctx->mChunkSize = chunkSize;
    ctx->mBuffer = malloc(sizeof(int16_t)*2*ctx->mChunkSize);
    ctx->mFramesOverlap = 0.5 * ctx->mChunkSize;
    ctx->mLock = malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(ctx->mLock, NULL);


    libvlc_media_player_set_media (ctx->mMp, ctx->mMedia);
    libvlc_media_player_play(ctx->mMp);

    return ctx;
}

int main(int argc, char **argv)
{
    if(argc < 2)
        printf("Usage: %s filename\n", argv[0]);
    else
    {
        vlc_context* ctx;
       
        ctx = caml_init_context(argv[1], 32);

        getchar();
    }

    return 0;
}


void useBuffer(vlc_context* ctx)
{
    int i;
    for(i = 0; i < ctx->mChunkSize; i++)
        printf("%d\n", ctx->mBuffer[i]);
}

// Get ready to render the stream to the buffer
void prepareRender( void* p_audio_data, uint8_t** pp_pcm_buffer , unsigned int size )
{
    vlc_context *sp = ((vlc_context*)p_audio_data);

    pthread_mutex_lock(sp->mLock);

    if(sp->mAudioDataSize < size)
    {
        if(sp->mAudioData)
            free(sp->mAudioData);
        sp->mAudioData = malloc(sizeof(char)*size); // Deleted in the destructor
    }
    *pp_pcm_buffer = (uint8_t*)(sp->mAudioData);
}

int min(int a, int b)
{
    return (a < b ? a : b);
}

void handleStream(void* p_audio_data, uint8_t* p_pcm_buffer, unsigned int channels, unsigned int rate,
                  unsigned int nb_samples, unsigned int bits_per_sample, unsigned int size, int64_t pts )
{
    unsigned int copied = 0;
    vlc_context *sp = ((vlc_context*)p_audio_data);

    // Update the frequency if needed
    if(rate != sp->mFrequency)
        sp->mFrequency = rate;
    sp->mChannels = channels;

    // The data is sent to us as bytes, but encoded on 2 bytes
    // TODO: dynamicly check that this is the case and that we're not mishandling the data
    int16_t* temp = (int16_t*)p_pcm_buffer;
    size /= 2;

    // We implemented a mechanism that takes the data sent by libVLC and cut it into chunks
    // of the same size (a power of two) so that the algorithms can handle it in the right way
    while(copied < size)
    {
        unsigned int to_copy = min(channels*(sp->mChunkSize - sp->mBufferSize), size - copied);
        memcpy(sp->mBuffer + channels*sp->mBufferSize, temp + copied,
                to_copy*sizeof(int16_t));
        copied += to_copy;
        sp->mBufferSize += to_copy / channels;

        if(sp->mBufferSize >= sp->mChunkSize)
        {
            // The buffer is sent to the "user"
            useBuffer(sp);

            // Emptying buffer
            flushBuffer(sp);
        }
    }

    pthread_mutex_unlock(sp->mLock);
}

void flushBuffer(vlc_context* ctx)
{
    memcpy(ctx->mBuffer, ctx->mBuffer + ctx->mChannels*(ctx->mChunkSize - ctx->mFramesOverlap),
            ctx->mChannels*ctx->mFramesOverlap*sizeof(int16_t));
    ctx->mBufferSize = ctx->mFramesOverlap;
}



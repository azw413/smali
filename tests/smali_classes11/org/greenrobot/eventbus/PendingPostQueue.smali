.class final Lorg/greenrobot/eventbus/PendingPostQueue;
.super Ljava/lang/Object;
.source "PendingPostQueue.java"


# instance fields
.field private head:Lorg/greenrobot/eventbus/PendingPost;

.field private tail:Lorg/greenrobot/eventbus/PendingPost;


# direct methods
.method constructor <init>()V
    .locals 0

    .line 19
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    return-void
.end method


# virtual methods
.method declared-synchronized enqueue(Lorg/greenrobot/eventbus/PendingPost;)V
    .locals 2
    .param p1, "pendingPost"    # Lorg/greenrobot/eventbus/PendingPost;

    monitor-enter p0

    .line 24
    if-eqz p1, :cond_2

    .line 27
    :try_start_0
    iget-object v0, p0, Lorg/greenrobot/eventbus/PendingPostQueue;->tail:Lorg/greenrobot/eventbus/PendingPost;

    if-eqz v0, :cond_0

    .line 28
    iput-object p1, v0, Lorg/greenrobot/eventbus/PendingPost;->next:Lorg/greenrobot/eventbus/PendingPost;

    .line 29
    iput-object p1, p0, Lorg/greenrobot/eventbus/PendingPostQueue;->tail:Lorg/greenrobot/eventbus/PendingPost;

    goto :goto_0

    .line 30
    .end local p0    # "this":Lorg/greenrobot/eventbus/PendingPostQueue;
    :cond_0
    iget-object v0, p0, Lorg/greenrobot/eventbus/PendingPostQueue;->head:Lorg/greenrobot/eventbus/PendingPost;

    if-nez v0, :cond_1

    .line 31
    iput-object p1, p0, Lorg/greenrobot/eventbus/PendingPostQueue;->tail:Lorg/greenrobot/eventbus/PendingPost;

    iput-object p1, p0, Lorg/greenrobot/eventbus/PendingPostQueue;->head:Lorg/greenrobot/eventbus/PendingPost;

    .line 35
    :goto_0
    invoke-virtual {p0}, Ljava/lang/Object;->notifyAll()V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    .line 36
    monitor-exit p0

    return-void

    .line 33
    :cond_1
    :try_start_1
    new-instance v0, Ljava/lang/IllegalStateException;

    const-string v1, "Head present, but no tail"

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0

    .line 23
    .end local p1    # "pendingPost":Lorg/greenrobot/eventbus/PendingPost;
    :catchall_0
    move-exception p1

    goto :goto_1

    .line 25
    .restart local p1    # "pendingPost":Lorg/greenrobot/eventbus/PendingPost;
    :cond_2
    new-instance v0, Ljava/lang/NullPointerException;

    const-string v1, "null cannot be enqueued"

    invoke-direct {v0, v1}, Ljava/lang/NullPointerException;-><init>(Ljava/lang/String;)V

    throw v0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    .line 23
    .end local p1    # "pendingPost":Lorg/greenrobot/eventbus/PendingPost;
    :goto_1
    monitor-exit p0

    throw p1
.end method

.method declared-synchronized poll()Lorg/greenrobot/eventbus/PendingPost;
    .locals 2

    monitor-enter p0

    .line 39
    :try_start_0
    iget-object v0, p0, Lorg/greenrobot/eventbus/PendingPostQueue;->head:Lorg/greenrobot/eventbus/PendingPost;

    move-object v1, v0

    .line 40
    .local v1, "pendingPost":Lorg/greenrobot/eventbus/PendingPost;
    if-eqz v0, :cond_0

    .line 41
    iget-object v0, v0, Lorg/greenrobot/eventbus/PendingPost;->next:Lorg/greenrobot/eventbus/PendingPost;

    iput-object v0, p0, Lorg/greenrobot/eventbus/PendingPostQueue;->head:Lorg/greenrobot/eventbus/PendingPost;

    .line 42
    if-nez v0, :cond_0

    .line 43
    const/4 v0, 0x0

    iput-object v0, p0, Lorg/greenrobot/eventbus/PendingPostQueue;->tail:Lorg/greenrobot/eventbus/PendingPost;
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    .line 46
    .end local p0    # "this":Lorg/greenrobot/eventbus/PendingPostQueue;
    :cond_0
    monitor-exit p0

    return-object v1

    .line 38
    .end local v1    # "pendingPost":Lorg/greenrobot/eventbus/PendingPost;
    :catchall_0
    move-exception v0

    monitor-exit p0

    throw v0
.end method

.method declared-synchronized poll(I)Lorg/greenrobot/eventbus/PendingPost;
    .locals 2
    .param p1, "maxMillisToWait"    # I
    .annotation system Ldalvik/annotation/Throws;
        value = {
            Ljava/lang/InterruptedException;
        }
    .end annotation

    monitor-enter p0

    .line 50
    :try_start_0
    iget-object v0, p0, Lorg/greenrobot/eventbus/PendingPostQueue;->head:Lorg/greenrobot/eventbus/PendingPost;

    if-nez v0, :cond_0

    .line 51
    int-to-long v0, p1

    invoke-virtual {p0, v0, v1}, Ljava/lang/Object;->wait(J)V

    .line 53
    .end local p0    # "this":Lorg/greenrobot/eventbus/PendingPostQueue;
    :cond_0
    invoke-virtual {p0}, Lorg/greenrobot/eventbus/PendingPostQueue;->poll()Lorg/greenrobot/eventbus/PendingPost;

    move-result-object v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    monitor-exit p0

    return-object v0

    .line 49
    .end local p1    # "maxMillisToWait":I
    :catchall_0
    move-exception p1

    monitor-exit p0

    throw p1
.end method

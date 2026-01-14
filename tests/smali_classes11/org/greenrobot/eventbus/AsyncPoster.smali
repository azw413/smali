.class Lorg/greenrobot/eventbus/AsyncPoster;
.super Ljava/lang/Object;
.source "AsyncPoster.java"

# interfaces
.implements Ljava/lang/Runnable;
.implements Lorg/greenrobot/eventbus/Poster;


# instance fields
.field private final eventBus:Lorg/greenrobot/eventbus/EventBus;

.field private final queue:Lorg/greenrobot/eventbus/PendingPostQueue;


# direct methods
.method constructor <init>(Lorg/greenrobot/eventbus/EventBus;)V
    .locals 1
    .param p1, "eventBus"    # Lorg/greenrobot/eventbus/EventBus;

    .line 29
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    .line 30
    iput-object p1, p0, Lorg/greenrobot/eventbus/AsyncPoster;->eventBus:Lorg/greenrobot/eventbus/EventBus;

    .line 31
    new-instance v0, Lorg/greenrobot/eventbus/PendingPostQueue;

    invoke-direct {v0}, Lorg/greenrobot/eventbus/PendingPostQueue;-><init>()V

    iput-object v0, p0, Lorg/greenrobot/eventbus/AsyncPoster;->queue:Lorg/greenrobot/eventbus/PendingPostQueue;

    .line 32
    return-void
.end method


# virtual methods
.method public enqueue(Lorg/greenrobot/eventbus/Subscription;Ljava/lang/Object;)V
    .locals 2
    .param p1, "subscription"    # Lorg/greenrobot/eventbus/Subscription;
    .param p2, "event"    # Ljava/lang/Object;

    .line 35
    invoke-static {p1, p2}, Lorg/greenrobot/eventbus/PendingPost;->obtainPendingPost(Lorg/greenrobot/eventbus/Subscription;Ljava/lang/Object;)Lorg/greenrobot/eventbus/PendingPost;

    move-result-object v0

    .line 36
    .local v0, "pendingPost":Lorg/greenrobot/eventbus/PendingPost;
    iget-object v1, p0, Lorg/greenrobot/eventbus/AsyncPoster;->queue:Lorg/greenrobot/eventbus/PendingPostQueue;

    invoke-virtual {v1, v0}, Lorg/greenrobot/eventbus/PendingPostQueue;->enqueue(Lorg/greenrobot/eventbus/PendingPost;)V

    .line 37
    iget-object v1, p0, Lorg/greenrobot/eventbus/AsyncPoster;->eventBus:Lorg/greenrobot/eventbus/EventBus;

    invoke-virtual {v1}, Lorg/greenrobot/eventbus/EventBus;->getExecutorService()Ljava/util/concurrent/ExecutorService;

    move-result-object v1

    invoke-interface {v1, p0}, Ljava/util/concurrent/ExecutorService;->execute(Ljava/lang/Runnable;)V

    .line 38
    return-void
.end method

.method public run()V
    .locals 3

    .line 42
    iget-object v0, p0, Lorg/greenrobot/eventbus/AsyncPoster;->queue:Lorg/greenrobot/eventbus/PendingPostQueue;

    invoke-virtual {v0}, Lorg/greenrobot/eventbus/PendingPostQueue;->poll()Lorg/greenrobot/eventbus/PendingPost;

    move-result-object v0

    .line 43
    .local v0, "pendingPost":Lorg/greenrobot/eventbus/PendingPost;
    if-eqz v0, :cond_0

    .line 46
    iget-object v1, p0, Lorg/greenrobot/eventbus/AsyncPoster;->eventBus:Lorg/greenrobot/eventbus/EventBus;

    invoke-virtual {v1, v0}, Lorg/greenrobot/eventbus/EventBus;->invokeSubscriber(Lorg/greenrobot/eventbus/PendingPost;)V

    .line 47
    return-void

    .line 44
    :cond_0
    new-instance v1, Ljava/lang/IllegalStateException;

    const-string v2, "No pending post available"

    invoke-direct {v1, v2}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v1
.end method

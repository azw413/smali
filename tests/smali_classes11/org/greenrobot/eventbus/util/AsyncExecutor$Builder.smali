.class public Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;
.super Ljava/lang/Object;
.source "AsyncExecutor.java"


# annotations
.annotation system Ldalvik/annotation/EnclosingClass;
    value = Lorg/greenrobot/eventbus/util/AsyncExecutor;
.end annotation

.annotation system Ldalvik/annotation/InnerClass;
    accessFlags = 0x9
    name = "Builder"
.end annotation


# instance fields
.field private eventBus:Lorg/greenrobot/eventbus/EventBus;

.field private failureEventType:Ljava/lang/Class;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/lang/Class<",
            "*>;"
        }
    .end annotation
.end field

.field private threadPool:Ljava/util/concurrent/Executor;


# direct methods
.method private constructor <init>()V
    .locals 0

    .line 46
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    .line 47
    return-void
.end method

.method synthetic constructor <init>(Lorg/greenrobot/eventbus/util/AsyncExecutor$1;)V
    .locals 0
    .param p1, "x0"    # Lorg/greenrobot/eventbus/util/AsyncExecutor$1;

    .line 41
    invoke-direct {p0}, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;-><init>()V

    return-void
.end method


# virtual methods
.method public build()Lorg/greenrobot/eventbus/util/AsyncExecutor;
    .locals 1

    .line 65
    const/4 v0, 0x0

    invoke-virtual {p0, v0}, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->buildForScope(Ljava/lang/Object;)Lorg/greenrobot/eventbus/util/AsyncExecutor;

    move-result-object v0

    return-object v0
.end method

.method public buildForScope(Ljava/lang/Object;)Lorg/greenrobot/eventbus/util/AsyncExecutor;
    .locals 7
    .param p1, "executionContext"    # Ljava/lang/Object;

    .line 69
    iget-object v0, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->eventBus:Lorg/greenrobot/eventbus/EventBus;

    if-nez v0, :cond_0

    .line 70
    invoke-static {}, Lorg/greenrobot/eventbus/EventBus;->getDefault()Lorg/greenrobot/eventbus/EventBus;

    move-result-object v0

    iput-object v0, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->eventBus:Lorg/greenrobot/eventbus/EventBus;

    .line 72
    :cond_0
    iget-object v0, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->threadPool:Ljava/util/concurrent/Executor;

    if-nez v0, :cond_1

    .line 73
    invoke-static {}, Ljava/util/concurrent/Executors;->newCachedThreadPool()Ljava/util/concurrent/ExecutorService;

    move-result-object v0

    iput-object v0, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->threadPool:Ljava/util/concurrent/Executor;

    .line 75
    :cond_1
    iget-object v0, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->failureEventType:Ljava/lang/Class;

    if-nez v0, :cond_2

    .line 76
    const-class v0, Lorg/greenrobot/eventbus/util/ThrowableFailureEvent;

    iput-object v0, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->failureEventType:Ljava/lang/Class;

    .line 78
    :cond_2
    new-instance v0, Lorg/greenrobot/eventbus/util/AsyncExecutor;

    iget-object v2, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->threadPool:Ljava/util/concurrent/Executor;

    iget-object v3, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->eventBus:Lorg/greenrobot/eventbus/EventBus;

    iget-object v4, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->failureEventType:Ljava/lang/Class;

    const/4 v6, 0x0

    move-object v1, v0

    move-object v5, p1

    invoke-direct/range {v1 .. v6}, Lorg/greenrobot/eventbus/util/AsyncExecutor;-><init>(Ljava/util/concurrent/Executor;Lorg/greenrobot/eventbus/EventBus;Ljava/lang/Class;Ljava/lang/Object;Lorg/greenrobot/eventbus/util/AsyncExecutor$1;)V

    return-object v0
.end method

.method public eventBus(Lorg/greenrobot/eventbus/EventBus;)Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;
    .locals 0
    .param p1, "eventBus"    # Lorg/greenrobot/eventbus/EventBus;

    .line 60
    iput-object p1, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->eventBus:Lorg/greenrobot/eventbus/EventBus;

    .line 61
    return-object p0
.end method

.method public failureEventType(Ljava/lang/Class;)Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;
    .locals 0
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Ljava/lang/Class<",
            "*>;)",
            "Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;"
        }
    .end annotation

    .line 55
    .local p1, "failureEventType":Ljava/lang/Class;, "Ljava/lang/Class<*>;"
    iput-object p1, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->failureEventType:Ljava/lang/Class;

    .line 56
    return-object p0
.end method

.method public threadPool(Ljava/util/concurrent/Executor;)Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;
    .locals 0
    .param p1, "threadPool"    # Ljava/util/concurrent/Executor;

    .line 50
    iput-object p1, p0, Lorg/greenrobot/eventbus/util/AsyncExecutor$Builder;->threadPool:Ljava/util/concurrent/Executor;

    .line 51
    return-object p0
.end method

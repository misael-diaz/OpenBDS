c defines MACROS to make the code more readable and easier to maintain:

c arguments of procedure virtpos():
#define VP_ARG \
      x,\
      y,\
      z,\
      of_x,\
      of_y,\
      of_z,\
      rj_x,\
      rj_y,\
      rj_z

c arguments of procedure relpos():
#define RP_ARG \
      ri_x,\
      ri_y,\
      ri_z,\
      rj_x,\
      rj_y,\
      rj_z,\
      rij_x,\
      rij_y,\
      rij_z

c arguments of procedure dist():
#define D_ARG \
      rij_x,\
      rij_y,\
      rij_z,\
      rij2,\
      rij

c arguments of procedure callback_SLJ_handler_dest():
#define CB_SLJ_HD_ARG \
      particles,\
      F_x,\
      F_y,\
      F_z,\
      rij_x,\
      rij_y,\
      rij_z,\
      rij,\
      Fij,\
      rij7,\
      rij8_inv,\
      rc7_inv

c defines alias for convenience
#define CBSLJHD_ARG CB_SLJ_HD_ARG

c arguments of procedure SLJ_force_dist_ratio_calc():
#define SLJ_FDRC_ARG \
      rij,\
      Fij,\
      rij7,\
      rij8_inv,\
      rc7_inv

c defines alias for convenience
#define SLJFDRC_ARG SLJ_FDRC_ARG

c arguments of procedure SLJ_force_nointeract_zero():
#define SLJ_FNIZ_ARG \
      i,\
      rij,\
      Fij

c defines alias for convenience
#define SLJFNIZ_ARG SLJ_FNIZ_ARG

c arguments of procedure SLJ_force_resultant():
#define SLJ_FR_ARG \
      rij_x,\
      rij_y,\
      rij_z,\
      Fij,\
      Fi_x,\
      Fi_y,\
      Fi_z

c defines alias for convenience
#define SLJFR_ARG SLJ_FR_ARG

c arguments of procedure SLJ_force():
#define SLJ_F_ARG \
      i,\
      rij_x,\
      rij_y,\
      rij_z,\
      rij,\
      Fi_x,\
      Fi_y,\
      Fi_z,\
      Fij,\
      rij7,\
      rij8_inv,\
      rc7_inv

c defines alias for convenience
#define SLJF_ARG SLJ_F_ARG

c arguments of procedure brute_force_dest():
#define BFDEST_ARG \
      particles,\
      x,\
      y,\
      z,\
      F_x,\
      F_y,\
      F_z,\
      ri_x,\
      ri_y,\
      ri_z,\
      of_x,\
      of_y,\
      of_z,\
      rj_x,\
      rj_y,\
      rj_z,\
      rij_x,\
      rij_y,\
      rij_z,\
      rij,\
      rij2

c arguments of procedure brute_force_virt():
#define BFVIRT_ARG \
      offset_x,\
      offset_y,\
      offset_z,\
      x,\
      y,\
      z,\
      of_x,\
      of_y,\
      of_z,\
      rj_x,\
      rj_y,\
      rj_z

c arguments of procedure brute_force_vec():
#define BFVEC_ARG \
      i,\
      x,\
      y,\
      z,\
      ri_x,\
      ri_y,\
      ri_z

c arguments of procedure brute_force_relpos():
#define BFRP_ARG RP_ARG

c arguments of procedure brute_force_dist():
#define BFDIST_ARG D_ARG

c arguments of procedure brute_force_iter():
#define BFI_ARG \
      particles,\
      i,\
      x,\
      y,\
      z,\
      rj_x,\
      rj_y,\
      rj_z,\
      ri_x,\
      ri_y,\
      ri_z,\
      rij_x,\
      rij_y,\
      rij_z,\
      rij,\
      rij2

c arguments of procedure brute_force_loop():
#define BFL_ARG \
      particles,\
      x,\
      y,\
      z,\
      rj_x,\
      rj_y,\
      rj_z,\
      ri_x,\
      ri_y,\
      ri_z,\
      rij_x,\
      rij_y,\
      rij_z,\
      rij,\
      rij2

c arguments of procedure brute_force():
#define BF_ARG \
      particles,\
      offset_x,\
      offset_y,\
      offset_z

c arguments of procedure brute_force_offset():
#define BFO_ARG BF_ARG

c arguments of procedure brute_force_driver_dest():
#define BFDD_ARG \
      particles,\
      F_x,\
      F_y,\
      F_z

      module force
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        use, intrinsic :: iso_fortran_env, only: i8 => int64
        use, intrinsic :: iso_fortran_env, only: r8 => real64
        use :: config, only: CLAMP
        use :: config, only: SPH_CONTACT
        use :: config, only: SPH_INTERACT_RANGE
        use :: config, only: L => LENGTH
        use :: config, only: N => NUM_PARTICLES
        use :: config, only: rc => SPH_INTERACT_RANGE ! cutoff-radius rc
        use :: vector, only: vector__vec
        use :: vector, only: vector__add
        use :: vector, only: vector__diff
        use :: vector, only: vector__norm
        use :: particle, only: particle_t
        use :: random, only: fnrand
        implicit none
        private
        public :: force__Brownian_force
        public :: force__callback_SLJ_handler
        public :: force__brute_force

        interface force__Brownian_force
          module procedure Brownian_force_base
        end interface

        interface force__callback_SLJ_handler
          module procedure callback_SLJ_handler
        end interface

        interface force__brute_force
          module procedure brute_force_base
        end interface

        interface
c         obtains the virtual position-vector of the jth-particles
          pure module subroutine virtpos (VP_ARG)
c           position vector components
            real(r8), intent(in) :: x(N)
            real(r8), intent(in) :: y(N)
            real(r8), intent(in) :: z(N)
c           vectorized offset-vector components
            real(r8), intent(in) :: of_x(N)
            real(r8), intent(in) :: of_y(N)
            real(r8), intent(in) :: of_z(N)
c           virtual position-vector components of the jth-particles
            real(r8), intent(out) :: rj_x(N)
            real(r8), intent(out) :: rj_y(N)
            real(r8), intent(out) :: rj_z(N)
          end subroutine virtpos
c         NOTE:
c         We use `virtual' to emphasize that the jth-particles may be virtual (not actual)
c         particles as a consequence of the periodic boundaries of the system.
        end interface

        interface
c         computes the relative-position vector `rij' components
          pure module subroutine relpos (RP_ARG)
c           vectorized position-vector components of the ith-particle
            real(r8), intent(in) :: ri_x(N)
            real(r8), intent(in) :: ri_y(N)
            real(r8), intent(in) :: ri_z(N)
c           position-vector components of the jth-particles
            real(r8), intent(in) :: rj_x(N)
            real(r8), intent(in) :: rj_y(N)
            real(r8), intent(in) :: rj_z(N)
c           relative-position vector components
            real(r8), intent(out) :: rij_x(N)
            real(r8), intent(out) :: rij_y(N)
            real(r8), intent(out) :: rij_z(N)
          end subroutine relpos
        end interface

        interface
c         computes the (squared) magnitude of the relative-position vector `rij'
          pure module subroutine dist (D_ARG)
c           relative-position vector components
            real(r8), intent(in) :: rij_x(N)
            real(r8), intent(in) :: rij_y(N)
            real(r8), intent(in) :: rij_z(N)
c           squared magnitude of the relative-position vector `rij'
            real(r8), intent(out) :: rij2(N)
c           magnitude of the relative-position vector `rij'
            real(r8), intent(out) :: rij(N)
          end subroutine dist
        end interface

        interface
c         computes the ratio of the SLJ-force magnitude `Fij' to the distance `rij'
          pure module subroutine SLJ_force_dist_ratio_calc (SLJFDRC_ARG)
            real(r8), intent(in) :: rij(N)
            real(r8), intent(out) :: Fij(N)
c           placeholders for intermediate computations
            real(r8), intent(out) :: rij7(N)
            real(r8), intent(out) :: rij8_inv(N)
            real(r8), intent(out) :: rc7_inv(N)
          end subroutine SLJ_force_dist_ratio_calc
        end interface

        interface
c         zeros the force `Fij' on non-interacting pairs of particles
          elemental module subroutine force_nointeract_zero (rij, Fij)
            real(r8), intent(in) :: rij
            real(r8), intent(inout) :: Fij
          end subroutine
        end interface

        interface
c         zeros the SLJ-force (to distance ratio) of non-interacting pairs of particles
          pure module subroutine SLJ_force_nointeract_zero (SLJFNIZ_ARG)
            integer(i8), intent(in) :: i
            real(r8), intent(in) :: rij(N)
            real(r8), intent(inout) :: Fij(N)
          end subroutine SLJ_force_nointeract_zero
        end interface

        interface
c         updates the resultant SLJ-force on the ith-particle
          pure module subroutine SLJ_force_resultant (SLJFR_ARG)
            real(r8), intent(in) :: rij_x(N)
            real(r8), intent(in) :: rij_y(N)
            real(r8), intent(in) :: rij_z(N)
            real(r8), intent(in) :: Fij(N)
            real(r8), intent(inout) :: Fi_x
            real(r8), intent(inout) :: Fi_y
            real(r8), intent(inout) :: Fi_z
          end subroutine SLJ_force_resultant
        end interface

        interface
c         drives the computation of the SLJ-force on the ith-particle
          pure module subroutine SLJ_force (SLJF_ARG)
c           ith-particle identifier ID
            integer(i8), intent(in) :: i
c           relative-position vector components
            real(r8), intent(in) :: rij_x(N)
            real(r8), intent(in) :: rij_y(N)
            real(r8), intent(in) :: rij_z(N)
            real(r8), intent(in) :: rij(N)
c           force vector components of the ith-particle
            real(r8), intent(inout) :: Fi_x
            real(r8), intent(inout) :: Fi_y
            real(r8), intent(inout) :: Fi_z
c           placeholders for intermediate computations
            real(r8), intent(out) :: Fij(N)
            real(r8), intent(out) :: rij7(N)
            real(r8), intent(out) :: rij8_inv(N)
            real(r8), intent(out) :: rc7_inv(N)
          end subroutine SLJ_force
        end interface

        interface
c         clamps a component of the force vector
          elemental module subroutine clamper (F_x)
            real(r8), intent(inout) :: F_x
          end subroutine clamper
        end interface

        interface
c         clamps the force vector so that its maximum does not exceed `CLAMP'
          pure module subroutine force_clamp (F_x, F_y, F_z)
            real(r8), intent(inout) :: F_x(N)
            real(r8), intent(inout) :: F_y(N)
            real(r8), intent(inout) :: F_z(N)
          end subroutine force_clamp
        end interface

        interface
c         destructures the needed fields (or properties) of the `particles' object
          pure module subroutine callback_SLJ_handler_dest (CBSLJHD_ARG)
            class(particle_t), intent(inout), target :: particles
c           force vector components
            real(r8), pointer, contiguous, intent(out) :: F_x(:)
            real(r8), pointer, contiguous, intent(out) :: F_y(:)
            real(r8), pointer, contiguous, intent(out) :: F_z(:)
c           relative-position vector components
            real(r8), pointer, contiguous, intent(out) :: rij_x(:)
            real(r8), pointer, contiguous, intent(out) :: rij_y(:)
            real(r8), pointer, contiguous, intent(out) :: rij_z(:)
c           relative-position vector magnitudes
            real(r8), pointer, contiguous, intent(out) :: rij(:)
c           temporary placeholders
            real(r8), pointer, contiguous, intent(out) :: Fij(:)
            real(r8), pointer, contiguous, intent(out) :: rij7(:)
            real(r8), pointer, contiguous, intent(out) :: rij8_inv(:)
            real(r8), pointer, contiguous, intent(out) :: rc7_inv(:)
          end subroutine callback_SLJ_handler_dest
        end interface

        interface
c         handles the callback to compute the SLJ force on the ith-particle
          pure module subroutine callback_SLJ_handler (particles, i)
            class(particle_t), intent(inout) :: particles
            integer(i8), intent(in) :: i ! ith-particle identifier ID
          end subroutine
        end interface

        interface
c         destructures the needed fields (or properties) of the `particles' object
          pure module subroutine brute_force_dest (BFDEST_ARG)
            class(particle_t), intent(inout), target :: particles
c           position vector components
            real(r8), pointer, contiguous, intent(out) :: x(:)
            real(r8), pointer, contiguous, intent(out) :: y(:)
            real(r8), pointer, contiguous, intent(out) :: z(:)
c           force vector components
            real(r8), pointer, contiguous, intent(out) :: F_x(:)
            real(r8), pointer, contiguous, intent(out) :: F_y(:)
            real(r8), pointer, contiguous, intent(out) :: F_z(:)
c           vectorized position-vector components of the ith-particle
            real(r8), pointer, contiguous, intent(out) :: ri_x(:)
            real(r8), pointer, contiguous, intent(out) :: ri_y(:)
            real(r8), pointer, contiguous, intent(out) :: ri_z(:)
c           vectorized offset-vector components
            real(r8), pointer, contiguous, intent(out) :: of_x(:)
            real(r8), pointer, contiguous, intent(out) :: of_y(:)
            real(r8), pointer, contiguous, intent(out) :: of_z(:)
c           virtual position-vector components of the jth-particles
            real(r8), pointer, contiguous, intent(out) :: rj_x(:)
            real(r8), pointer, contiguous, intent(out) :: rj_y(:)
            real(r8), pointer, contiguous, intent(out) :: rj_z(:)
c           relative-position vector components
            real(r8), pointer, contiguous, intent(out) :: rij_x(:)
            real(r8), pointer, contiguous, intent(out) :: rij_y(:)
            real(r8), pointer, contiguous, intent(out) :: rij_z(:)
c           magnitude of the relative-position vector `rij'
            real(r8), pointer, contiguous, intent(out) :: rij(:)
c           squared magnitude of the relative-position vector `rij'
            real(r8), pointer, contiguous, intent(out) :: rij2(:)
          end subroutine brute_force_dest
        end interface

        interface
c         destructures the needed fields (or properties) of the `particles' object
          pure module subroutine brute_force_driver_dest (BFDD_ARG)
            class(particle_t), intent(inout), target :: particles
c           force vector components
            real(r8), pointer, contiguous, intent(out) :: F_x(:)
            real(r8), pointer, contiguous, intent(out) :: F_y(:)
            real(r8), pointer, contiguous, intent(out) :: F_z(:)
          end subroutine
        end interface

        interface
c         computes the position vectors of the virtual jth-particles that interact with
c         the ith-particle
          pure module subroutine brute_force_virt (BFVIRT_ARG)
c           offset along the axes
            real(r8), intent(in) :: offset_x
            real(r8), intent(in) :: offset_y
            real(r8), intent(in) :: offset_z
c           position vector components
            real(r8), intent(in) :: x(N)
            real(r8), intent(in) :: y(N)
            real(r8), intent(in) :: z(N)
c           vectorized offset-vector components
            real(r8), intent(out) :: of_x(N)
            real(r8), intent(out) :: of_y(N)
            real(r8), intent(out) :: of_z(N)
c           virtual position-vector components
            real(r8), intent(out) :: rj_x(N)
            real(r8), intent(out) :: rj_y(N)
            real(r8), intent(out) :: rj_z(N)
          end subroutine brute_force_virt
        end interface

        interface
c         vectorizes the position vector components of the ith-particle
          pure module subroutine brute_force_vec (BFVEC_ARG)
c           ith-particle identifier ID
            integer(i8), intent(in) :: i
c           position vector components
            real(r8), intent(in) :: x(N)
            real(r8), intent(in) :: y(N)
            real(r8), intent(in) :: z(N)
c           vectorized position vector components of the ith-particle
            real(r8), intent(out) :: ri_x(N)
            real(r8), intent(out) :: ri_y(N)
            real(r8), intent(out) :: ri_z(N)
          end subroutine brute_force_vec
        end interface

        interface
c         obtains the relative-position vector of the i-j particle pairs
          pure module subroutine brute_force_relpos (BFRP_ARG)
c           vectorized position-vector components of the ith-particle
            real(r8), intent(in) :: ri_x(N)
            real(r8), intent(in) :: ri_y(N)
            real(r8), intent(in) :: ri_z(N)
c           position-vector components of the jth-particles
            real(r8), intent(in) :: rj_x(N)
            real(r8), intent(in) :: rj_y(N)
            real(r8), intent(in) :: rj_z(N)
c           relative-position vector components
            real(r8), intent(out) :: rij_x(N)
            real(r8), intent(out) :: rij_y(N)
            real(r8), intent(out) :: rij_z(N)
          end subroutine brute_force_relpos
        end interface

        interface
c         obtains the (squared) relative-distances between the i-j particle pairs
          pure module subroutine brute_force_dist (BFDIST_ARG)
c           relative-position vector components
            real(r8), intent(in) :: rij_x(N)
            real(r8), intent(in) :: rij_y(N)
            real(r8), intent(in) :: rij_z(N)
c           squared magnitude of the relative-position vector `rij'
            real(r8), intent(out) :: rij2(N)
c           magnitude of the relative-position vector `rij'
            real(r8), intent(out) :: rij(N)
          end subroutine brute_force_dist
        end interface

        interface
c         updates the resultant force on the ith-particle
          pure module subroutine brute_force_iter (BFI_ARG)
            class(particle_t), intent(inout) :: particles
c           ith-particle identifier ID
            integer(i8), intent(in) :: i
c           position vector components
            real(r8), intent(in) :: x(N)
            real(r8), intent(in) :: y(N)
            real(r8), intent(in) :: z(N)
c           position vector components of the jth-particles
            real(r8), intent(in) :: rj_x(N)
            real(r8), intent(in) :: rj_y(N)
            real(r8), intent(in) :: rj_z(N)
c           (vectorized) position vector components of the ith-particle
            real(r8), intent(out) :: ri_x(N)
            real(r8), intent(out) :: ri_y(N)
            real(r8), intent(out) :: ri_z(N)
c           relative-position vector components
            real(r8), intent(out) :: rij_x(N)
            real(r8), intent(out) :: rij_y(N)
            real(r8), intent(out) :: rij_z(N)
            real(r8), intent(out) :: rij2(N)
            real(r8), intent(out) :: rij(N)
          end subroutine brute_force_iter
        end interface

        interface
c         updates the force on the ith-particles exerted by the jth virtual particles
          pure module subroutine brute_force_loop (BFL_ARG)
            class(particle_t), intent(inout) :: particles
c           position vector components
            real(r8), intent(in) :: x(N)
            real(r8), intent(in) :: y(N)
            real(r8), intent(in) :: z(N)
c           position-vector components of the jth-particles
            real(r8), intent(in) :: rj_x(N)
            real(r8), intent(in) :: rj_y(N)
            real(r8), intent(in) :: rj_z(N)
c           vectorized position-vector components of the ith-particle
            real(r8), intent(out) :: ri_x(N)
            real(r8), intent(out) :: ri_y(N)
            real(r8), intent(out) :: ri_z(N)
c           relative-position vector components
            real(r8), intent(out) :: rij_x(N)
            real(r8), intent(out) :: rij_y(N)
            real(r8), intent(out) :: rij_z(N)
            real(r8), intent(out) :: rij(N)
            real(r8), intent(out) :: rij2(N)
          end subroutine brute_force_loop
        end interface

        interface
c         computes the forces exerted by the virtual particles on the actual particles
          pure module subroutine brute_force (BF_ARG)
            class(particle_t), intent(inout) :: particles
            real(r8), intent(in) :: offset_x
            real(r8), intent(in) :: offset_y
            real(r8), intent(in) :: offset_z
          end subroutine brute_force
c         NOTE: intent(inout) variables may be (indirectly) modified by the callback
        end interface

        interface
c         forwards the task to the brute_force() procedure
          pure module subroutine brute_force_offset (BFO_ARG)
            class(particle_t), intent(inout) :: particles
            real(r8), intent(in) :: offset_x
            real(r8), intent(in) :: offset_y
            real(r8), intent(in) :: offset_z
          end subroutine brute_force_offset
        end interface

        interface
c         constructs the particle images along some axis (or direction) `ax'
          pure module subroutine brute_force_img (particles, ax)
            class(particle_t), intent(inout) :: particles
            character(len=1), intent(in) :: ax
          end subroutine
        end interface

        interface
c         drives the computation of the interparticle forces
          pure module subroutine brute_force_driver (particles)
            class(particle_t), intent(inout) :: particles
          end subroutine
        end interface

        interface
c         base code, forwards its task to the driver
          pure module subroutine brute_force_base (particles)
            class(particle_t), intent(inout) :: particles
          end subroutine
        end interface
      contains

        impure elemental subroutine Brownian_force_component (F_x)
c         Synopsis:
c         Fills the Brownian force component (elementwise) with normally distributed
c         pseudo-random numbers.
          real(kind = real64), intent(out) :: F_x

          F_x = fnrand()

          return
        end subroutine Brownian_force_component


        subroutine Brownian_force (F_x, F_y, F_z)
c         Synopsis:
c         Fills the Brownian forces with normally distributed pseudo-random numbers.
          real(kind = real64), intent(out) :: F_x(N)
          real(kind = real64), intent(out) :: F_y(N)
          real(kind = real64), intent(out) :: F_z(N)

          call Brownian_force_component(F_x)
          call Brownian_force_component(F_y)
          call Brownian_force_component(F_z)

          return
        end subroutine Brownian_force


        subroutine Brownian_force_base (particles)
c         Synopsis:
c         Updates the force vectors with the Brownian forces.
          class(particle_t), intent(inout), target :: particles
          real(kind = real64), pointer, contiguous :: F_x(:) => null()
          real(kind = real64), pointer, contiguous :: F_y(:) => null()
          real(kind = real64), pointer, contiguous :: F_z(:) => null()

          F_x => particles % F_x
          F_y => particles % F_y
          F_z => particles % F_z

          call Brownian_force(F_x, F_y, F_z)

          return
        end subroutine Brownian_force_base

      end module force

c     NOTE:
c     if the code gets much larger we can relocate the submodule elsewhere to reduce
c     the compilation time if that becomes a concern
      submodule (force) brute_force_methods
        implicit none
      contains

        module procedure virtpos

          call vector__add(VP_ARG)

          return
        end procedure virtpos


        module procedure relpos

          call vector__diff(RP_ARG)

          return
        end procedure relpos


        module procedure dist

          call vector__norm(D_ARG)

          return
        end procedure dist


        module procedure SLJ_force_dist_ratio_calc
          real(r8), parameter :: C6 = (SPH_CONTACT**6)
          real(r8), parameter :: SLJ_EPSILON = 1.0_r8
          real(r8), parameter :: SLJ_EPS = SLJ_EPSILON
          real(r8), parameter :: kappa = (4.0_r8 * SLJ_EPS * C6)
          real(r8), parameter :: k = (6.0_r8 * kappa)

          rij7 = rij**7
          rc7_inv = rc**(-7)
          rij8_inv = rij**(-8)

c         we break down the computation of the following equation in the next lines:
c               Fij = k * ( rij8_inv - (rc7_inv * rij7) * rij8_inv )
c         note that the compiler can fuse the implicit loops if profitable (or optimal)

          Fij = rc7_inv * rij7
          rij7 = Fij * rij8_inv
          Fij = rij8_inv - rij7
          rc7_inv = k
          rij7 = rc7_inv * Fij
          Fij = rij7

          return
        end procedure SLJ_force_dist_ratio_calc


        module procedure force_nointeract_zero

          if (rij < SPH_INTERACT_RANGE) then
            Fij = 1.0_r8 * Fij
          else
            Fij = 0.0_r8
          end if

          return
        end procedure force_nointeract_zero


        module procedure SLJ_force_nointeract_zero

          Fij(i) = 0.0_r8

          call force_nointeract_zero(rij, Fij)

          return
        end procedure SLJ_force_nointeract_zero


        module procedure SLJ_force_resultant

          Fi_x = Fi_x + sum(Fij * rij_x)
          Fi_y = Fi_y + sum(Fij * rij_y)
          Fi_z = Fi_z + sum(Fij * rij_z)

          return
        end procedure SLJ_force_resultant


        module procedure SLJ_force

          call SLJ_force_dist_ratio_calc(SLJ_FDRC_ARG)
          call SLJ_force_nointeract_zero(SLJ_FNIZ_ARG)
          call SLJ_force_resultant(SLJ_FR_ARG)

          return
        end procedure SLJ_force


        module procedure clamper

          if (F_x > CLAMP) then
            F_x = +CLAMP
          elseif (F_x < -CLAMP) then
            F_x = -CLAMP
          else
            F_x = 1.0_r8 * F_x
          end if

          return
        end procedure clamper


        module procedure force_clamp

          call clamper(F_x)
          call clamper(F_y)
          call clamper(F_z)

          return
        end procedure force_clamp


        module procedure callback_SLJ_handler_dest
c         size `sz' of the preceding (contiguous) memory block
          integer(i8) :: sz

          F_x => particles % F_x
          F_y => particles % F_y
          F_z => particles % F_z

          Fij => particles % T_x
          rij7 => particles % T_y
          rij8_inv => particles % T_z

          sz = 6_i8 * N
          rij_x => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rij_y => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rij_z => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rij => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rc7_inv => particles % tmp(1 + sz:N + sz)

          return
        end procedure callback_SLJ_handler_dest


        module procedure brute_force_dest
c         size `sz' of the preceding (contiguous) memory block
          integer(i8) :: sz

          x => particles % x
          y => particles % y
          z => particles % z

          F_x => particles % F_x
          F_y => particles % F_y
          F_z => particles % F_z

          ri_x => particles % T_x
          ri_y => particles % T_y
          ri_z => particles % T_z

          sz = 0_i8
          of_x => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          of_y => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          of_z => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rj_x => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rj_y => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rj_z => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rij_x => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rij_y => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rij_z => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rij => particles % tmp(1 + sz:N + sz)

          sz = sz + N
          rij2 => particles % tmp(1 + sz:N + sz)

          return
        end procedure brute_force_dest


        module procedure brute_force_driver_dest

          F_x => particles % F_x
          F_y => particles % F_y
          F_z => particles % F_z

          return
        end procedure brute_force_driver_dest


        module procedure callback_SLJ_handler
c         (read-write) force vector components of the ith-particle
          real(r8), pointer :: Fi_x
          real(r8), pointer :: Fi_y
          real(r8), pointer :: Fi_z
c         (read-write) force vector components
          real(r8), pointer, contiguous :: F_x(:)
          real(r8), pointer, contiguous :: F_y(:)
          real(r8), pointer, contiguous :: F_z(:)
c         (read-only) relative position vector components
          real(r8), pointer, contiguous :: rij_x(:)
          real(r8), pointer, contiguous :: rij_y(:)
          real(r8), pointer, contiguous :: rij_z(:)
          real(r8), pointer, contiguous :: rij(:)
c         (write-read) temporary placeholders
          real(r8), pointer, contiguous :: Fij(:)
          real(r8), pointer, contiguous :: rij7(:)
          real(r8), pointer, contiguous :: rij8_inv(:)
          real(r8), pointer, contiguous :: rc7_inv(:)

          call callback_SLJ_handler_dest(CB_SLJ_HD_ARG)

          Fi_x => F_x(i)
          Fi_y => F_y(i)
          Fi_z => F_z(i)

          call SLJ_force(SLJF_ARG)

          return
        end procedure callback_SLJ_handler


        module procedure brute_force_virt

          call vector__vec(offset_x,
     +                     offset_y,
     +                     offset_z,
     +                     of_x,
     +                     of_y,
     +                     of_z)

          call virtpos(VP_ARG)

          return
        end procedure brute_force_virt


        module procedure brute_force_vec
          real(r8) :: x_i
          real(r8) :: y_i
          real(r8) :: z_i

          x_i = x(i)
          y_i = y(i)
          z_i = z(i)

          call vector__vec(x_i, y_i, z_i, ri_x, ri_y, ri_z)

        end procedure brute_force_vec


        module procedure brute_force_relpos

          call relpos(RP_ARG)

          return
        end procedure brute_force_relpos


        module procedure brute_force_dist

          call dist(D_ARG)

          return
        end procedure brute_force_dist


        module procedure brute_force_iter

          call brute_force_vec(BFVEC_ARG)
          call brute_force_relpos(BFRP_ARG)
          call brute_force_dist(BFDIST_ARG)
          call particles % callback(i)

          return
        end procedure brute_force_iter


        module procedure brute_force_loop
c         ith-particle identifier ID
          integer(i8) :: i

          do i = 1_i8, N

            call brute_force_iter(BFI_ARG)

          end do

          return
        end procedure brute_force_loop


        module procedure brute_force
c         (read-only) position vector components
          real(r8), pointer, contiguous :: x(:)
          real(r8), pointer, contiguous :: y(:)
          real(r8), pointer, contiguous :: z(:)
c         (read-write) force vector components
          real(r8), pointer, contiguous :: F_x(:)
          real(r8), pointer, contiguous :: F_y(:)
          real(r8), pointer, contiguous :: F_z(:)
c         (write-read) vectorized position-vector component of the ith particle
          real(r8), pointer, contiguous :: ri_x(:)
          real(r8), pointer, contiguous :: ri_y(:)
          real(r8), pointer, contiguous :: ri_z(:)
c         (write-read) offset-vector components
          real(r8), pointer, contiguous :: of_x(:)
          real(r8), pointer, contiguous :: of_y(:)
          real(r8), pointer, contiguous :: of_z(:)
c         (write-read) virtual-position vector components
          real(r8), pointer, contiguous :: rj_x(:)
          real(r8), pointer, contiguous :: rj_y(:)
          real(r8), pointer, contiguous :: rj_z(:)
c         (write-read) relative-position vector components
          real(r8), pointer, contiguous :: rij_x(:)
          real(r8), pointer, contiguous :: rij_y(:)
          real(r8), pointer, contiguous :: rij_z(:)
c         (write-read) squared relative distances
          real(r8), pointer, contiguous :: rij2(:)
c         (write-read) relative distances
          real(r8), pointer, contiguous :: rij(:)

          call brute_force_dest(BFDEST_ARG)
          call brute_force_virt(BFVIRT_ARG)
          call brute_force_loop(BFL_ARG)

          return
        end procedure brute_force


        module procedure brute_force_offset

          call brute_force(BF_ARG)

          return
        end procedure brute_force_offset


        module procedure brute_force_img
          real(r8) :: offset_x
          real(r8) :: offset_y
          real(r8) :: offset_z
          real(r8) :: o_x
          real(r8) :: o_y
          real(r8) :: o_z

          offset_x = 0.0_r8
          offset_y = 0.0_r8
          offset_z = 0.0_r8

          o_x = offset_x
          o_y = offset_y
          o_z = offset_z

          select case (ax)
          case ('X')
            o_x = L
            call brute_force_offset(particles,-o_x, o_y, o_z)
            call brute_force_offset(particles,+o_x, o_y, o_z)
          case ('Y')
            o_y = L
            call brute_force_offset(particles, o_x,-o_y, o_z)
            call brute_force_offset(particles, o_x,+o_y, o_z)
          case ('Z')
            o_z = L
            call brute_force_offset(particles, o_x, o_y,-o_z)
            call brute_force_offset(particles, o_x, o_y,+o_z)
          case default
            call brute_force_offset(particles, o_x, o_y, o_z)
          end select

          return
        end procedure brute_force_img


        module procedure brute_force_driver
c         (write-read) force vector components
          real(r8), pointer, contiguous :: F_x(:)
          real(r8), pointer, contiguous :: F_y(:)
          real(r8), pointer, contiguous :: F_z(:)

          call brute_force_driver_dest(BFDD_ARG)

          F_x = 0.0_r8
          F_y = 0.0_r8
          F_z = 0.0_r8

          call brute_force_img(particles = particles, ax = 'O')
          call brute_force_img(particles = particles, ax = 'X')
          call brute_force_img(particles = particles, ax = 'Y')
          call brute_force_img(particles = particles, ax = 'Z')

          call force_clamp(F_x, F_y, F_z)

          return
        end procedure brute_force_driver


        module procedure brute_force_base

          call brute_force_driver(particles)

          return
        end procedure brute_force_base

      end submodule brute_force_methods

*   OpenBDS                                             October 24, 2023
*
*   source: api/fortran/module/force/force.f
*   author: @misael-diaz
*
*   Synopsis:
*   Implements methods for computing the forces on the particles.
*
*   Copyright (C) 2023 Misael Díaz-Maldonado
*
*   This program is free software: you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation, either version 3 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program. If not, see <http://www.gnu.org/licenses/>.
*
*   References:
*   [0] SJ Chapman, FORTRAN for Scientists and Engineers, 4th edition.
*   [1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
*   [2] S Kim and S Karrila, Microhydrodynamics.

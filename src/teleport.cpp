#include "teleport.h"

#include <cmath>
#include <memory>
#include <string>

#include "avatar.h"
#include "calendar.h"
#include "character.h"
#include "creature.h"
#include "creature_tracker.h"
#include "debug.h"
#include "do_turn.h"
#include "effect.h"
#include "enums.h"
#include "event.h"
#include "event_bus.h"
#include "explosion.h"
#include "game.h"
#include "map.h"
#include "messages.h"
#include "point.h"
#include "rng.h"
#include "submap.h"
#include "tileray.h"
#include "translations.h"
#include "type_id.h"
#include "vehicle.h"
#include "viewer.h"
#include "map_iterator.h"
#include "ui_manager.h"

static const efftype_id effect_teleglow( "teleglow" );

static const flag_id json_flag_DIMENSIONAL_ANCHOR( "DIMENSIONAL_ANCHOR" );
static const flag_id json_flag_GRAB( "GRAB" );
static const flag_id json_flag_TELEPORT_LOCK( "TELEPORT_LOCK" );

bool teleport::teleport( Creature &critter, int min_distance, int max_distance, bool safe,
                         bool add_teleglow )
{
    if( min_distance > max_distance ) {
        debugmsg( "ERROR: Function teleport::teleport called with invalid arguments." );
        return false;
    }
    int tries = 0;
    tripoint origin = critter.pos();
    tripoint new_pos = origin;
    map &here = get_map();
    do {
        int rangle = rng( 0, 360 );
        int rdistance = rng( min_distance, max_distance );
        new_pos.x = origin.x + rdistance * std::cos( rangle );
        new_pos.y = origin.y + rdistance * std::sin( rangle );
        tries++;
    } while( here.impassable( new_pos ) && tries < 20 );
    return teleport_to_point( critter, new_pos, safe, add_teleglow );
}

bool teleport::teleport_to_point( Creature &critter, tripoint target, bool safe,
                                  bool add_teleglow, bool display_message, bool force )
{
    if( critter.pos() == target ) {
        return false;
    }
    Character *const p = critter.as_character();
    const bool c_is_u = p != nullptr && p->is_avatar();
    map &here = get_map();
    tripoint_abs_ms abs_ms( here.getabs( target ) );
    //The teleportee is dimensionally anchored so nothing happens
    if( !force && p && ( p->worn_with_flag( json_flag_DIMENSIONAL_ANCHOR ) ||
                         p->has_effect_with_flag( json_flag_DIMENSIONAL_ANCHOR ) ||
                         p->has_effect_with_flag( json_flag_TELEPORT_LOCK ) ) ) {
        if( display_message ) {
            p->add_msg_if_player( m_warning, _( "You feel a strange, inwards force." ) );
        }
        return false;
    }
    if( p && p->in_vehicle ) {
        here.unboard_vehicle( p->pos_bub() );
    }
    map tm;
    map *dest = &here;
    tripoint dest_target = target;
    if( !here.inbounds( target ) ) {
        if( c_is_u ) {
            g->place_player_overmap( project_to<coords::omt>( abs_ms ), false );
        } else {
            dest = &tm;
            dest->load( project_to<coords::sm>( abs_ms ), false );
            dest->spawn_monsters( true, true );
        }
        dest_target = dest->getlocal( abs_ms );
    }
    //handles teleporting into solids.
    if( dest->impassable( dest_target ) ) {
        if( force ) {
            const std::optional<tripoint> nt =
                random_point( points_in_radius( dest_target, 5 ),
            [dest]( const tripoint & el ) {
                return dest->passable( el );
            } );
            dest_target = nt ? *nt : dest_target;
        } else {
            if( safe ) {
                if( c_is_u && display_message ) {
                    add_msg( m_bad, _( "You cannot teleport safely." ) );
                }
                return false;
            }
            critter.apply_damage( nullptr, bodypart_id( "torso" ), 9999 );
            if( c_is_u ) {
                get_event_bus().send<event_type::teleports_into_wall>( p->getID(),
                        dest->obstacle_name( dest_target ) );
                if( display_message ) {
                    add_msg( m_bad, _( "You die after teleporting into a solid." ) );
                }
            }
            critter.check_dead_state();
        }
    }
    //update pos
    abs_ms = dest->getglobal( dest_target );
    target = here.getlocal( abs_ms );
    //handles telefragging other creatures
    int tfrag_attempts = 5;
    bool collision = false;
    int collision_angle = 0;
    while( Creature *const poor_soul = get_creature_tracker().creature_at<Creature>( abs_ms ) ) {
        //Fail if we run out of telefrag attempts
        if( tfrag_attempts-- < 1 ) {
            if( p && display_message ) {
                p->add_msg_player_or_npc( m_warning, _( "You flicker." ), _( "<npcname> flickers." ) );
            } else if( get_player_view().sees( critter ) && display_message ) {
                add_msg( _( "%1$s flickers." ), critter.disp_name() );
            }
            return false;
        }
        //if the thing that was going to be teleported into has a dimensional anchor, break out early and don't teleport.
        if( poor_soul->as_character() &&
            ( poor_soul->as_character()->worn_with_flag( json_flag_DIMENSIONAL_ANCHOR ) ||
              poor_soul->as_character()->has_effect_with_flag( json_flag_DIMENSIONAL_ANCHOR ) ) ) {
            poor_soul->as_character()->add_msg_if_player( m_warning, _( "You feel disjointed." ) );
            return false;
        }
        if( force ) {
            //this should only happen through debug menu, so this won't affect the player.
            poor_soul->apply_damage( nullptr, bodypart_id( "torso" ), 9999 );
            poor_soul->check_dead_state();
        } else if( safe ) {
            if( c_is_u && display_message ) {
                add_msg( m_bad, _( "You cannot teleport safely." ) );
            }
            return false;
        } else if( !collision ) {
            //we passed all the conditions needed for a teleport accident, so handle messages for teleport accidents here
            const bool poor_soul_is_u = poor_soul->is_avatar();
            if( poor_soul_is_u && display_message ) {
                add_msg( m_bad, _( "You're blasted with strange energy!" ) );
            }
            if( p ) {
                if( display_message ) {
                    p->add_msg_player_or_npc( m_warning,
                                              _( "You collide with %s mid teleport, and you are both knocked away by a violent explosion of energy." ),
                                              _( "<npcname> collides with %s mid teleport, and they are both knocked away by a violent explosion of energy." ),
                                              poor_soul->disp_name() );
                }
            } else {
                if( get_player_view().sees( *poor_soul ) ) {
                    if( display_message ) {
                        add_msg( m_warning,
                                 _( "%1$s collides with %2$s mid teleport, and they are both knocked away by a violent explosion of energy!" ),
                                 critter.disp_name(), poor_soul->disp_name() );
                    }
                }
                //once collision this if block shouldn't run so everything here should only happen once
                collision = true;
                //determine a random angle to throw the thing it teleported into, then fling it.
                collision_angle = rng( 0, 360 );
                g->fling_creature( poor_soul, units::from_degrees( collision_angle - 180 ), 40, false, true );
                //spawn a mostly cosmetic explosion for flair.
                explosion_handler::explosion( &critter, target, 10 );
                //if it was grabbed, it isn't anymore.
                for( const effect &grab : poor_soul->get_effects_with_flag( json_flag_GRAB ) ) {
                    poor_soul->remove_effect( grab.get_id() );
                }
                //apply a bunch of damage to it, similar to a tear in reality
                poor_soul->apply_damage( nullptr, bodypart_id( "arm_l" ), rng( 5, 10 ) );
                poor_soul->apply_damage( nullptr, bodypart_id( "arm_r" ), rng( 5, 10 ) );
                poor_soul->apply_damage( nullptr, bodypart_id( "leg_l" ), rng( 7, 12 ) );
                poor_soul->apply_damage( nullptr, bodypart_id( "leg_r" ), rng( 7, 12 ) );
                poor_soul->apply_damage( nullptr, bodypart_id( "torso" ), rng( 5, 15 ) );
                poor_soul->apply_damage( nullptr, bodypart_id( "head" ), rng( 2, 8 ) );
                poor_soul->check_dead_state();
            }
        }
    }
    critter.move_to( abs_ms );
    //there was a collision with a creature at some point, so handle that.
    if( collision ) {
        //throw the thing that teleported in the opposite direction as the thing it teleported into.
        g->fling_creature( &critter, units::from_degrees( collision_angle - 180 ), 40, false, true );
        //do a bunch of damage to it too.
        critter.apply_damage( nullptr, bodypart_id( "arm_l" ), rng( 5, 10 ) );
        critter.apply_damage( nullptr, bodypart_id( "arm_r" ), rng( 5, 10 ) );
        critter.apply_damage( nullptr, bodypart_id( "leg_l" ), rng( 7, 12 ) );
        critter.apply_damage( nullptr, bodypart_id( "leg_r" ), rng( 7, 12 ) );
        critter.apply_damage( nullptr, bodypart_id( "torso" ), rng( 5, 15 ) );
        critter.apply_damage( nullptr, bodypart_id( "head" ), rng( 2, 8 ) );
        critter.check_dead_state();
    }
    //player and npc exclusive teleporting effects
    if( p && add_teleglow ) {
        p->add_effect( effect_teleglow, 30_minutes );
    }
    if( c_is_u ) {
        g->place_player( p->pos() );
        g->update_map( *p );
    }
    for( const effect &grab : critter.get_effects_with_flag( json_flag_GRAB ) ) {
        critter.remove_effect( grab.get_id() );
    }
    return true;
}

bool teleport::teleport_vehicle( vehicle &veh, const tripoint_abs_ms &dp )
{
    const std::set<int>& parts_to_move = {};
    
    map &here = get_map();
    map *dest = &here;
    tileray facing;
    facing.init( veh.turn_dir );



    veh.precalc_mounts( 1, veh.skidding ? veh.turn_dir : facing.dir(), veh.pivot_point() );

    // cancel out any movement of the vehicle due only to a change in pivot
    //tripoint_rel_ms dp1 = dp - veh.pivot_displacement();

    Character &player_character = get_player_character();
    //const bool seen = here.sees_veh( player_character, veh, false );




    const tripoint_bub_ms src = veh.pos_bub();
    //const tripoint_abs_ms src = veh.global_square_location();
    // handle vehicle ramps
    int ramp_offset = 0;
    //if( adjust_pos ) {
    //    if( has_flag( ter_furn_flag::TFLAG_RAMP_UP, src + dp ) ) {
    //        ramp_offset += 1;
    //        veh.is_on_ramp = true;
    //    } else if( has_flag( ter_furn_flag::TFLAG_RAMP_DOWN, src + dp ) ) {
    //        ramp_offset -= 1;
    //        veh.is_on_ramp = true;
    //    }
    //}


    if( !here.inbounds( src ) ) {
        /*      add_msg_debug( debugmode::DF_MAP,
                             "map::displace_vehicle: coordinates out of bounds %d,%d,%d->%d,%d,%d",
                             src.x(), src.y(), src.z(), dst.x(), dst.y(), dst.z() );*/
        //return false;
    }

    map tm;
    point_sm_ms src_offset;
    point_sm_ms dst_offset;
    submap *src_submap = here.get_submap_at( src, src_offset );
    submap *dst_submap;
    if( dest->inbounds( dp ) ) {
        dst_submap = dest->get_submap_at( dest->bub_from_abs( dp ), dst_offset );
    } else {
        dest = &tm;
        dest->load( project_to<coords::sm>( dp ), false );
        dst_submap = dest->get_submap_at( dest->bub_from_abs( dp ), dst_offset );
        if( dst_submap == nullptr ) {
            debugmsg( "Tried to displace vehicle at (%d,%d) but the dest submap is not loaded", dst_offset.x(),
                      dst_offset.y() );
            return true;
        }
    }
    if( src_submap == nullptr ) {
        debugmsg( "Tried to displace vehicle at (%d,%d) but the src submap is not loaded", src_offset.x(),
                  src_offset.y() );
        return true;
    }
    std::set<int> smzs;

    // first, let's find our position in current vehicles vector
    size_t our_i = 0;
    bool found = false;
    for( submap *&smap : here.grid ) {
        for( size_t i = 0; i < smap->vehicles.size(); i++ ) {
            if( smap->vehicles[i].get() == &veh ) {
                our_i = i;
                src_submap = smap;
                found = true;
                break;
            }
        }
        if( found ) {
            break;
        }
    }

    if( !found ) {
        add_msg_debug( debugmode::DF_MAP, "displace_vehicle [%s] failed", veh.name );
        return false;
    }


    here.memory_clear_vehicle_points( veh );

    // Need old coordinates to check for remote control
    const bool remote = veh.remote_controlled( player_character );

    // record every passenger and pet inside
    std::vector<rider_data> riders = veh.get_riders();

    bool need_update = false;
    int z_change = 0;
    // Move passengers and pets
    bool complete = false;
    creature_tracker &creatures = get_creature_tracker();
    // loop until everyone has moved or for each passenger
    for( size_t i = 0; !complete && i < riders.size(); i++ ) {
        complete = true;
        for( rider_data &r : riders ) {
            if( r.moved ) {
                continue;
            }
            const int prt = r.prt;
            if( !parts_to_move.empty() && parts_to_move.find( prt ) == parts_to_move.end() ) {
                r.moved = true;
                continue;
            }
            Creature *psg = r.psg;
            const tripoint_bub_ms part_pos = veh.bub_part_pos( prt );
            if( psg == nullptr ) {
                debugmsg( "Empty passenger for part #%d at %d,%d,%d player at %d,%d,%d?",
                          prt, part_pos.x(), part_pos.y(), part_pos.z(),
                          player_character.posx(), player_character.posy(), player_character.posz() );
                veh.part( prt ).remove_flag( vp_flag::passenger_flag );
                r.moved = true;
                continue;
            }

            if( psg->pos_bub() != part_pos ) {
                add_msg_debug( debugmode::DF_MAP, "Part/passenger position mismatch: part #%d at %d,%d,%d "
                               "passenger at %d,%d,%d", prt, part_pos.x(), part_pos.y(), part_pos.z(),
                               psg->posx(), psg->posy(), psg->posz() );
            }
            const vehicle_part &veh_part = veh.part( prt );

            // ramps make everything super tricky
            int psg_offset_z = -ramp_offset;
            tripoint next_pos; // defaults to 0,0,0
            if( parts_to_move.empty() ) {
                next_pos = veh_part.precalc[1];
            }
            //if( has_flag( ter_furn_flag::TFLAG_RAMP_UP, src + dp + next_pos ) ) {
            //    psg_offset_z += 1;
            //} else if( has_flag( ter_furn_flag::TFLAG_RAMP_DOWN, src + dp + next_pos ) ) {
            //    psg_offset_z -= 1;
            //}

            // Place passenger on the new part location
            tripoint_bub_ms psgp( here.bub_from_abs( dp ) + next_pos + tripoint( 0, 0, psg_offset_z ) );
            // someone is in the way so try again
            if( creatures.creature_at( psgp ) ) {
                complete = false;
                continue;
            }
            if( psg->is_avatar() ) {
                // If passenger is you, we need to update the map
                need_update = true;
                z_change = psgp.z() - part_pos.z();
            }

            psg->setpos( psgp.raw() );
            r.moved = true;
        }
    }


    smzs = veh.advance_precalc_mounts( dst_offset.raw(), src.raw(), dp.raw(), ramp_offset,
                                       true, parts_to_move );
    veh.update_active_fakes();

    if( src_submap != dst_submap ) {
        dst_submap->ensure_nonuniform();
        veh.set_submap_moved( tripoint( here.bub_from_abs( dp ).x() / SEEX,
                                        here.bub_from_abs( dp ).y() / SEEY, dp.z() ) );
        auto src_submap_veh_it = src_submap->vehicles.begin() + our_i;
        dst_submap->vehicles.push_back( std::move( *src_submap_veh_it ) );
        src_submap->vehicles.erase( src_submap_veh_it );
        here.invalidate_max_populated_zlev( dp.z() );
    }
    if( need_update ) {
        g->update_map( player_character );
    }
    dest->add_vehicle_to_cache( &veh );

    if( z_change || src.z() != dp.z() ) {
        if( z_change ) {
            g->vertical_move( z_change, true );
            // vertical moves can flush the caches, so make sure we're still in the cache
            dest->add_vehicle_to_cache( &veh );
        }
        dest->update_vehicle_list( dst_submap, dp.z() );
        // delete the vehicle from the source z-level vehicle cache set if it is no longer on
        // that z-level
        if( src.z() != dp.z() ) {
            
        }
        veh.check_is_heli_landed();
    }
    level_cache &ch2 = here.get_cache( src.z() );
            for( const vehicle *elem : ch2.vehicle_list ) {
                if( elem == &veh ) {
                    ch2.vehicle_list.erase( &veh );
                    ch2.zone_vehicles.erase( &veh );
                    break;
                }
            }
    if( remote ) {
        // Has to be after update_map or coordinates won't be valid
        g->setremoteveh( &veh );
    }

    veh.zones_dirty = true; // invalidate zone positions

    for( int vsmz : smzs ) {
        here.on_vehicle_moved( dp.z() + vsmz );
    }









    if( veh.is_towing() ) {
        add_msg( m_info, _( "A towing cable snaps off of %s." ),
                 veh.tow_data.get_towed()->disp_name() );
        veh.tow_data.get_towed()->invalidate_towing( true );
    }
        g->invalidate_main_ui_adaptor();
        ui_manager::redraw_invalidated();
        handle_key_blocking_activity();

    here.invalidate_map_cache( src.z() );
    return true;
}
